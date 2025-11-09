// src/main.rs

use clap::Parser;
use minilang::compile_source;
use std::{fs, io::{self, Read}};
// 警告が出ていた TypedFunc を削除
use wasmi::{Engine, Linker, Module, Store, Value};

/// コマンドライン引数を定義するための構造体
// ... (Cli構造体の定義は変更なし) ...
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

    let wasm_binary = wat::parse_str(wat_code)?;
    
    let module = Module::new(&engine, &*wasm_binary)?;
    
    let linker = <Linker<()>>::new(&engine);
    let instance = linker
        .instantiate(&mut store, &module)?
        .start(&mut store)?;
    
    let execute_func = instance
        .get_func(&store, "execute")
        .ok_or("Wasmモジュールに関数 'execute' が見つかりません")?;

    // 【修正点】 0.0 を .into() で wasmi_core::F64 型に変換する
    let mut results = [Value::F64(0.0.into())];

    execute_func.call(&mut store, &[], &mut results)?;
    
    let result = match &results[0] {
        // 【修正点】 valは wasmi_core::F64 型なので、.to_float() で f64 に戻す
        Value::F64(val) => val.to_float(),
        other => return Err(format!("予期しない戻り値の型です: {:?}", other).into()),
    };

    println!("実行結果: {}", result);

    Ok(())
}

// ... (main関数は変更なし) ...
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