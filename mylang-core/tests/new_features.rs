use mylang_core::analyze_source;
use mylang_core::ast::{DataType, TypedAstNode, TypedExprKind, TypedPattern};

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
