use mylang_core::ast::RawAstNode;
use mylang_core::error::DiagnosticKind;
use mylang_core::{lex_source, parse_tokens_with_diagnostics};

#[test]
fn parser_reports_multiple_errors_and_recovers() {
    let source = "let 10;\nstruct { }\nlet ok 1;";
    let tokens = lex_source(source).expect("lexing should succeed");
    let report = parse_tokens_with_diagnostics(&tokens);
    assert!(report.has_errors(), "expected parse errors");
    assert!(
        report.diagnostics.len() >= 2,
        "expected at least two diagnostics, got {}",
        report.diagnostics.len()
    );
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.message.contains("identifier"))
    );
    let has_ok_binding = report.nodes.iter().any(|node| match node {
        RawAstNode::Let { name, .. } => name.0 == "ok",
        RawAstNode::LetHoist { name, .. } => name.0 == "ok",
        _ => false,
    });
    assert!(has_ok_binding, "parser should keep succeeding statements");
}

#[test]
fn redundant_semicolons_emit_warning() {
    let source = "fn sample ||->i32 { let value 1;; value };";
    let tokens = lex_source(source).expect("lexing should succeed");
    let report = parse_tokens_with_diagnostics(&tokens);
    assert!(
        !report.has_errors(),
        "function with redundant semicolons should still parse"
    );
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.kind == DiagnosticKind::Warning
                && diag.message.contains("Redundant ';'")),
        "expected redundant semicolon warning"
    );
    assert!(
        !report.nodes.is_empty(),
        "expected AST nodes to be produced"
    );
}
