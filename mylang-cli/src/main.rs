use clap::Parser;
use mylang_core::analyze_source;
use std::{
    fs,
    io::{self, Read, Write},
    process,
};
// wasmi 0.51 に合わせたuse文
use wasmi::{core::Trap, Caller, Engine, Linker, Module, Store};

/// コマンドライン引数を定義するための構造体
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    input: Option<String>,

    #[arg(short, long)]
    output: String,

    #[arg(long, value_name = "FORMAT", default_value = "wasm", help = "Output format: wasm, llvm")]
    emit: String,

    #[arg(long, help = "Run the code if the output format is wasm")]
    run: bool,
}

/// Wasmの実行結果を処理するメイン関数
fn run_wasm(wat_code: &str) -> Result<(), Box<dyn std::error::Error>> {
    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let mut linker = <Linker<()>>::new(&engine);

    // WASIのfd_writeをホスト側で実装する
    linker.func_wrap(
        "wasi_snapshot_preview1",
        "fd_write",
        |mut caller: Caller<'_, ()>,
         fd: i32,
         iovecs_ptr: i32,
         iovecs_len: i32,
         nwritten_ptr: i32|
         -> Result<i32, Trap> {
            let memory = caller
                .get_export("memory")
                .and_then(|ext| ext.into_memory())
                .ok_or_else(|| Trap::new("failed to find memory export"))?;

            if fd != 1 && fd != 2 {
                const ERRNO_NOTSUP: i32 = 58;
                return Ok(ERRNO_NOTSUP);
            }

            let mut written_bytes: u32 = 0;
            let iovecs_ptr = iovecs_ptr as u32;

            for i in 0..iovecs_len {
                let iovec_offset = (iovecs_ptr + (i * 8) as u32) as usize;
                let mut buf_ptr_bytes = [0u8; 4];
                memory
                    .read(&caller, iovec_offset, &mut buf_ptr_bytes)
                    .map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_ptr = u32::from_le_bytes(buf_ptr_bytes);

                let mut buf_len_bytes = [0u8; 4];
                memory
                    .read(&caller, iovec_offset + 4, &mut buf_len_bytes)
                    .map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_len = u32::from_le_bytes(buf_len_bytes);

                let data = memory
                    .data(&caller)
                    .get(buf_ptr as usize..(buf_ptr + buf_len) as usize)
                    .ok_or_else(|| Trap::new("buffer out of bounds"))?;

                io::stdout()
                    .write_all(data)
                    .map_err(|e| Trap::new(format!("failed to write to stdout: {}", e)))?;
                written_bytes += buf_len;
            }

            io::stdout()
                .flush()
                .map_err(|e| Trap::new(format!("failed to flush stdout: {}", e)))?;

            memory
                .write(
                    &mut caller,
                    nwritten_ptr as usize,
                    &written_bytes.to_le_bytes(),
                )
                .map_err(|_| Trap::new("pointer out of bounds"))?;

            const ERRNO_SUCCESS: i32 = 0;
            Ok(ERRNO_SUCCESS)
        },
    )?;

    let wasm_binary = wat::parse_str(wat_code)?;
    let module = Module::new(&engine, &*wasm_binary)?;
    
    // モジュールをインスタンス化する
    let instance = linker.instantiate(&mut store, &module)?.start(&mut store)?;

    // _start関数を取得して呼び出す
    let start_func = instance
        .get_func(&store, "_start")
        .ok_or("Wasmモジュールに関数 '_start' が見つかりません")?;

    start_func.call(&mut store, &[], &mut [])?;

    println!("\n実行が正常に終了しました。");

    Ok(())
}

fn main() {
    if let Err(e) = try_main() {
        eprintln!("Error: {}", e);
        process::exit(1);
    }
}

fn try_main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // --- 1. 入力コードの読み込み ---
    let mut source_code = String::new();
    match cli.input {
        Some(path) => {
            source_code = fs::read_to_string(&path)
                .map_err(|e| format!("ファイル '{}' の読み込みに失敗しました: {}", path, e))?;
        }
        None => {
            io::stdin().read_to_string(&mut source_code)?;
        }
    }

    // --- 2. フロントエンドによる解析 ---
    println!("解析中...");
    let analysis_result = analyze_source(source_code.trim())
        .map_err(|e| format!("コンパイルエラー: {}", e))?;

    // --- 3. バックエンドの選択とコード生成 ---
    println!("コード生成中 ({}) ...", cli.emit);
    let output_code = match cli.emit.as_str() {
        "wasm" => {
            let mut wasm_generator = mylang_wasm_codegen::WasmGenerator::new(
                analysis_result.function_table,
                analysis_result.string_headers,
                analysis_result.static_offset,
            );
            wasm_generator
                .generate(&analysis_result.typed_ast)
                .map_err(|e| format!("Wasm code generation error: {}", e))?
        }

        #[cfg(feature = "llvm")]
        "llvm" => mylang_llvm_codegen::generate(&analysis_result)
            .map_err(|e| format!("LLVM code generation error: {}", e))?,

        #[cfg(not(feature = "llvm"))]
        "llvm" => {
            return Err(
                "LLVM output is not supported in this build. Recompile with the 'llvm' feature."
                    .into(),
            );
        }
        _ => {
            return Err(format!(
                "Unknown emit format: '{}'. Supported formats: wasm, llvm.",
                cli.emit
            )
            .into());
        }
    };

    // --- 4. ファイル書き出し & 実行 ---
    fs::write(&cli.output, &output_code).map_err(|e| {
        format!(
            "ファイル '{}' への書き込みに失敗しました: {}",
            &cli.output, e
        )
    })?;
    println!(
        "'{}' に {} を出力しました。",
        &cli.output,
        cli.emit.to_uppercase()
    );

    if cli.run {
        if cli.emit == "wasm" {
            println!("Wasmを実行します...");
            if let Err(e) = run_wasm(&output_code) {
                eprintln!("実行時エラー: {}", e);
            }
        } else {
            println!("'--run' is only supported for the 'wasm' emit format.");
        }
    }

    Ok(())
}