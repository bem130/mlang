use mylang_core::analyze_source;
use mylang_core::ast::{DataType, TypedAstNode, TypedExprKind, TypedPattern};
use mylang_core::error::LangError;

#[test]
fn struct_enum_tuple_match_are_supported() {
    let source = r#"
struct Point {
    x: i32,
    y: i32,
}

enum Toggle {
    On,
    Off,
}

fn match_tuple(flag: bool) -> i32 {
    let coords = if flag { (1, 2) } else { (3, 4) };
    match coords {
        (x, y) => $x + y$,
    }
}
"#;

    let analysis = analyze_source(source).expect("analysis should succeed");
    assert!(
        analysis
            .typed_ast
            .iter()
            .any(|node| matches!(node, TypedAstNode::StructDef { name, .. } if name == "Point"))
    );
    assert!(
        analysis
            .typed_ast
            .iter()
            .any(|node| matches!(node, TypedAstNode::EnumDef { name, .. } if name == "Toggle"))
    );

    let function = analysis
        .typed_ast
        .iter()
        .find_map(|node| match node {
            TypedAstNode::FnDef { name, body, .. } if name == "match_tuple" => Some(body),
            _ => None,
        })
        .expect("function definition present");

    let TypedExprKind::Block { statements } = &function.kind else {
        panic!("function body should be a block");
    };

    let match_expr = statements
        .last()
        .expect("match expression as last statement");

    assert_eq!(match_expr.data_type, DataType::I32);

    let TypedExprKind::Match { value, arms } = &match_expr.kind else {
        panic!("expected match expression");
    };
    assert_eq!(
        value.data_type,
        DataType::Tuple(vec![DataType::I32, DataType::I32])
    );
    assert_eq!(arms.len(), 1);

    let first_arm = &arms[0];
    match &first_arm.pattern {
        TypedPattern::Tuple(elements) => {
            assert_eq!(elements.len(), 2);
            for element in elements {
                match element {
                    TypedPattern::Binding { data_type, .. } => {
                        assert_eq!(data_type, &DataType::I32);
                    }
                    other => panic!("unexpected pattern element: {:?}", other),
                }
            }
        }
        other => panic!("unexpected pattern: {:?}", other),
    }

    match &first_arm.body.kind {
        TypedExprKind::FunctionCall { name, args } => {
            assert_eq!(name, "i32.add");
            assert_eq!(args.len(), 2);
            for arg in args {
                assert_eq!(arg.data_type, DataType::I32);
            }
        }
        other => panic!("unexpected match arm body: {:?}", other),
    }
}

#[test]
fn nested_tuple_patterns_are_parsed_recursively() {
    let source = r#"
fn destructure(flag: bool) -> i32 {
    let pair = if flag { (1, (2, 3)) } else { (4, (5, 6)) };
    match pair {
        (a, (b, c)) => $a + b + c$,
    }
}
"#;

    let analysis = analyze_source(source).expect("analysis should succeed");

    let function_body = analysis
        .typed_ast
        .iter()
        .find_map(|node| match node {
            TypedAstNode::FnDef { name, body, .. } if name == "destructure" => Some(body),
            _ => None,
        })
        .expect("function definition present");

    let TypedExprKind::Block { statements } = &function_body.kind else {
        panic!("function body should be a block");
    };

    let match_expr = statements.last().expect("match expression present");

    let TypedExprKind::Match { arms, .. } = &match_expr.kind else {
        panic!("expected match expression");
    };

    assert_eq!(arms.len(), 1);
    let TypedPattern::Tuple(outer_elements) = &arms[0].pattern else {
        panic!("expected tuple pattern");
    };
    assert_eq!(outer_elements.len(), 2);

    match (&outer_elements[0], &outer_elements[1]) {
        (
            TypedPattern::Binding {
                data_type: first_type,
                ..
            },
            TypedPattern::Tuple(inner_elements),
        ) => {
            assert_eq!(first_type, &DataType::I32);
            assert_eq!(inner_elements.len(), 2);
            for element in inner_elements {
                match element {
                    TypedPattern::Binding { data_type, .. } => {
                        assert_eq!(data_type, &DataType::I32);
                    }
                    other => panic!("unexpected inner pattern: {:?}", other),
                }
            }
        }
        other => panic!("unexpected outer patterns: {:?}", other),
    }
}

#[test]
fn non_exhaustive_enum_match_is_rejected() {
    let source = r#"
enum Toggle {
    On,
    Off,
}

fn choose(val: Toggle) -> i32 {
    match val {
        Toggle::On => 1,
    }
}
"#;

    let err = match analyze_source(source) {
        Ok(_) => panic!("analysis should fail"),
        Err(err) => err,
    };
    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error, got {:?}", err);
    };
    assert!(
        compile_err
            .message
            .contains("Non-exhaustive match on enum 'Toggle'"),
        "unexpected error message: {}",
        compile_err.message
    );
}

#[test]
fn non_exhaustive_bool_match_requires_all_arms() {
    let source = r#"
fn to_int(flag: bool) -> i32 {
    match flag {
        true => 1,
    }
}
"#;

    let err = match analyze_source(source) {
        Ok(_) => panic!("analysis should fail"),
        Err(err) => err,
    };
    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error, got {:?}", err);
    };
    assert!(
        compile_err
            .message
            .contains("Non-exhaustive match on 'bool'"),
        "unexpected error message: {}",
        compile_err.message
    );
}

#[test]
fn matches_on_infinite_types_require_wildcard_arm() {
    let source = r#"
fn kind(x: i32) -> i32 {
    match x {
        0 => 0,
    }
}
"#;

    let err = match analyze_source(source) {
        Ok(_) => panic!("analysis should fail"),
        Err(err) => err,
    };
    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error, got {:?}", err);
    };
    assert!(
        compile_err
            .message
            .contains("Match expression on type 'i32' is not exhaustive"),
        "unexpected error message: {}",
        compile_err.message
    );
}
