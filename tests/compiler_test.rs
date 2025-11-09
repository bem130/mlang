use minilang::error::LangError;
use std::fs;
use std::path::PathBuf;

/// 指定されたサブディレクトリ内のすべての`.mlang`ファイルに対してテストを実行するヘルパー関数。
///
/// # Arguments
/// * `subdir` - `tests/samples`内のテストファイルが格納されているディレクトリ名 (例: "passing")。
/// * `validator` - コンパイル結果を検証するクロージャ。
///   - 第1引数: `compile_source`の結果 (`Result<String, LangError>`) への参照。
///   - 第2引数: テスト対象のファイル名。
///   - このクロージャは、アサーションが失敗した場合に`panic!`を呼び出す責務を負う。
fn run_tests_in_dir<F>(subdir: &str, validator: F)
where
    F: Fn(&Result<String, LangError>, &str),
{
    // プロジェクトのルートからの相対パスを構築
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("tests/samples");
    path.push(subdir);

    let entries = fs::read_dir(&path)
        .unwrap_or_else(|e| panic!("Failed to read test directory '{}': {}", path.display(), e));

    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        // `.mlang` ファイルのみを対象とする
        if path.is_file() && path.extension().map_or(false, |s| s == "mlang") {
            let file_name = path.file_name().unwrap().to_str().unwrap();
            
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("Failed to read test file '{}': {}", file_name, e));
            
            // ソースコードをコンパイルし、結果をvalidatorクロージャに渡す
            let result = minilang::compile_source(&source);
            validator(&result, file_name);
        }
    }
}

/// `tests/samples/passing` ディレクトリ内のすべてのテストが、
/// エラーなくコンパイルできることを検証する。
#[test]
fn all_passing_tests_should_compile_successfully() {
    run_tests_in_dir("passing", |result, file_name| {
        if let Err(e) = result {
            panic!(
                "\n\n[FAIL] A 'passing' test case failed to compile.\n  File: {}\n  Error: {}\n\n",
                file_name, e
            );
        }
    });
}

/// `tests/samples/failing` ディレクトリ内のすべてのテストが、
/// 期待通りにコンパイルエラーになることを検証する。
#[test]
fn all_failing_tests_should_fail_to_compile() {
    run_tests_in_dir("failing", |result, file_name| {
        if result.is_ok() {
            panic!(
                "\n\n[FAIL] A 'failing' test case unexpectedly compiled successfully.\n  File: {}\n\n",
                file_name
            );
        }
    });
}