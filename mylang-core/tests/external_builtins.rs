extern crate alloc;

use alloc::vec;
use mylang_core::analyze_source_with_builtins;
use mylang_core::ast::DataType;
use mylang_core::builtins::{ProvidedFunction, stdio_builtins, wasm_builtins};

#[test]
fn allows_calling_wasm_provided_builtin() {
    let source = r#"
fn main ||->() {
    fd_write(1, 0, 0, 0);
}
"#;

    let analysis = analyze_source_with_builtins(source, false, &wasm_builtins())
        .expect("analysis should succeed");
    assert!(analysis.function_table.contains_key("fd_write"));
}

#[test]
fn ignores_duplicate_stdlib_registration() {
    let provided = stdio_builtins();
    let source = r#"
fn main ||->() {
    println("hello");
}
"#;

    let analysis =
        analyze_source_with_builtins(source, false, &provided).expect("analysis should succeed");
    let signatures = analysis
        .function_table
        .get("println")
        .expect("println should be registered");

    assert_eq!(1, signatures.len());
    assert_eq!(vec![DataType::String], signatures[0].param_types);
}

#[test]
fn can_mix_custom_and_built_in_registrations() {
    let mut provided = wasm_builtins();
    provided.push(ProvidedFunction::new(
        "host_exit",
        vec![DataType::I32],
        DataType::I32,
    ));

    let source = r#"
fn main ||->() {
    host_exit(fd_write(1, 0, 0, 0));
}
"#;

    let analysis =
        analyze_source_with_builtins(source, false, &provided).expect("analysis should succeed");

    let exit_signatures = analysis
        .function_table
        .get("host_exit")
        .expect("host_exit should be registered");
    assert_eq!(1, exit_signatures.len());
    assert_eq!(DataType::I32, exit_signatures[0].return_type);
}
