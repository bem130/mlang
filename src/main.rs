// src/main.rs

use clap::Parser;
use minilang::compile_source;
use std::{
    fs,
    io::{self, Read},
};
use wasmi::{Engine, Linker, Module, Store, Value};

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

    let wasm_binary = wat::parse_str(wat_code)?;

    let module = Module::new(&engine, &*wasm_binary)?;

    let linker = <Linker<()>>::new(&engine);
    let instance = linker.instantiate(&mut store, &module)?.start(&mut store)?;

    let execute_func = instance
        .get_func(&store, "execute")
        .ok_or("Wasmモジュールに関数 'execute' が見つかりません")?;

    // 仕様により、コンパイラは'main'関数の戻り値を常にi32としてWATを生成する。
    // そのため、実行環境は戻り値がi32であることを常に期待する。
    let mut result_buffer = [Value::I32(0)];
    execute_func.call(&mut store, &[], &mut result_buffer)?;
    
    if let Some(Value::I32(val)) = result_buffer.first() {
        println!("実行結果: {}", val);
    } else {
        // このケースはwasmiのAPIが期待通りに動作すれば到達しないはず
        return Err("実行時エラー: Wasmからの戻り値の取得に失敗しました。".into());
    }

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