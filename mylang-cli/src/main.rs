// mylang-cli/src/main.rs

use clap::Parser;
use mylang_core::compile_source;
use std::{
    fs,
    io::{self, Read, Write},
};
use wasmi::{core::Trap, Caller, Engine, Linker, Module, Store};

/// コマンドライン引数を定義するための構造体
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    #[arg(short, long)]
    input: Option<String>,
    #[arg(short, long)]
    output: String,
    #[arg(long)]
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

            // stdout(1)とstderr(2)以外はサポートしない
            if fd != 1 && fd != 2 {
                const ERRNO_NOTSUP: i32 = 58; // wasi::ERRNO_NOTSUP.raw()
                return Ok(ERRNO_NOTSUP);
            }
            
            let mut written_bytes: u32 = 0;
            let iovecs_ptr = iovecs_ptr as u32;

            for i in 0..iovecs_len {
                let iovec_offset = (iovecs_ptr + (i * 8) as u32) as usize;
                
                // iovecからbufのポインタを読み込む
                let mut buf_ptr_bytes = [0u8; 4];
                memory.read(&caller, iovec_offset, &mut buf_ptr_bytes).map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_ptr = u32::from_le_bytes(buf_ptr_bytes);

                // iovecからbufの長さを読み込む
                let mut buf_len_bytes = [0u8; 4];
                memory.read(&caller, iovec_offset + 4, &mut buf_len_bytes).map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_len = u32::from_le_bytes(buf_len_bytes);

                // メモリからデータを読み込んで標準出力に書き込む
                let data = memory.data(&caller).get(buf_ptr as usize..(buf_ptr + buf_len) as usize).ok_or_else(|| Trap::new("pointer out of bounds"))?;
                
                io::stdout().write_all(data).map_err(|_| Trap::new("failed to write to stdout"))?;
                written_bytes += buf_len;
            }

            // 即座に出力を反映させるためにflushする
            io::stdout().flush().map_err(|_| Trap::new("failed to flush stdout"))?;

            // 書き込んだバイト数をメモリに書き戻す
            memory.write(&mut caller, nwritten_ptr as usize, &written_bytes.to_le_bytes()).map_err(|_| Trap::new("pointer out of bounds"))?;
            
            const ERRNO_SUCCESS: i32 = 0; // wasi::ERRNO_SUCCESS.raw()
            Ok(ERRNO_SUCCESS)
        },
    )?;

    let wasm_binary = wat::parse_str(wat_code)?;
    let module = Module::new(&engine, &*wasm_binary)?;
    let instance = linker.instantiate(&mut store, &module)?.start(&mut store)?;

    // WASIでは_start関数がエントリーポイント
    let start_func = instance
        .get_func(&store, "_start")
        .ok_or("Wasmモジュールに関数 '_start' が見つかりません")?;

    // _startは引数も戻り値もない
    start_func.call(&mut store, &[], &mut [])?;
    
    println!("\n実行が正常に終了しました。");

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // コマンドライン引数をパース
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

    // --- 2. コンパイル ---
    println!("コンパイル中...");
    let wat_code = compile_source(source_code.trim())
        .map_err(|e| format!("コンパイルエラー: {}", e))?;

    // --- 3. WATファイルの書き出し ---
    fs::write(&cli.output, &wat_code)
        .map_err(|e| format!("ファイル '{}' への書き込みに失敗しました: {}", &cli.output, e))?;
    println!("'{}' にWATを出力しました。", &cli.output);

    // --- 4. 実行 ---
    if cli.run {
        println!("Wasmを実行します...");
        if let Err(e) = run_wasm(&wat_code) {
            eprintln!("実行時エラー: {}", e);
        }
    }

    Ok(())
}