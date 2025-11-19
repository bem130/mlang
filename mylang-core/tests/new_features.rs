use mylang_core::analyze_source;
use mylang_core::ast::{DataType, LiteralValue, TypedAstNode, TypedExpr, TypedExprKind};
use mylang_core::error::LangError;

// 新仕様に合わせてテストを書き直す必要あり

// #[test]
// fn struct_enum_tuple_match_are_supported() {
//     let source = r#"
// struct Point {
//     x: i32,
//     y: i32,
// }

// enum Toggle {
//     On,
//     Off,
// }

// fn match_tuple |flag: bool|->i32 {
//     let coords = if flag { (1, 2) } else { (3, 4) };
//     match coords {
//         (x, y) => $x + y$,
//     }
// };
// "#;

//     let analysis = analyze_source(source, true).expect("analysis should succeed");
//     assert!(
//         analysis
//             .typed_ast
//             .iter()
//             .any(|node| matches!(node, TypedAstNode::StructDef { name, .. } if name == "Point"))
//     );
//     assert!(
//         analysis
//             .typed_ast
//             .iter()
//             .any(|node| matches!(node, TypedAstNode::EnumDef { name, .. } if name == "Toggle"))
//     );

//     let function = analysis
//         .typed_ast
//         .iter()
//         .find_map(|node| match node {
//             TypedAstNode::FnDef { name, body, .. } if name == "match_tuple" => Some(body),
//             _ => None,
//         })
//         .expect("function definition present");

//     let TypedExprKind::Block { statements } = &function.kind else {
//         panic!("function body should be a block");
//     };

//     let match_expr = statements
//         .last()
//         .expect("match expression as last statement");

//     assert_eq!(match_expr.data_type, DataType::I32);

//     let TypedExprKind::Match { value, arms } = &match_expr.kind else {
//         panic!("expected match expression");
//     };
//     assert_eq!(
//         value.data_type,
//         DataType::Tuple(vec![DataType::I32, DataType::I32])
//     );
//     assert_eq!(arms.len(), 1);

//     let first_arm = &arms;
//     match &first_arm.pattern {
//         TypedPattern::Tuple(elements) => {
//             assert_eq!(elements.len(), 2);
//             for element in elements {
//                 match element {
//                     TypedPattern::Binding { data_type, .. } => {
//                         assert_eq!(data_type, DataType::I32);
//                     }
//                     other => panic!("unexpected pattern element: {:?}", other),
//                 }
//             }
//         }
//         other => panic!("unexpected pattern: {:?}", other),
//     }

//     match &first_arm.body.kind {
//         TypedExprKind::FunctionCall { name, args } => {
//             assert_eq!(name, "i32.add");
//             assert_eq!(args.len(), 2);
//             for arg in args {
//                 assert_eq!(arg.data_type, DataType::I32);
//             }
//         }
//         other => panic!("unexpected match arm body: {:?}", other),
//     }
// }

// #[test]
// fn nested_tuple_patterns_are_parsed_recursively() {
//     let source = r#"
// fn destructure |flag: bool|->i32 {
//     let pair = if flag { (1, (2, 3)) } else { (4, (5, 6)) };
//     match pair {
//         (a, (b, c)) => $a + b + c$,
//     }
// };
// "#;

//     let analysis = analyze_source(source, true).expect("analysis should succeed");

//     let function_body = analysis
//         .typed_ast
//         .iter()
//         .find_map(|node| match node {
//             TypedAstNode::FnDef { name, body, .. } if name == "destructure" => Some(body),
//             _ => None,
//         })
//         .expect("function definition present");

//     let TypedExprKind::Block { statements } = &function_body.kind else {
//         panic!("function body should be a block");
//     };

//     let match_expr = statements.last().expect("match expression present");

//     let TypedExprKind::Match { arms, .. } = &match_expr.kind else {
//         panic!("expected match expression");
//     };

//     assert_eq!(arms.len(), 1);
//     let TypedPattern::Tuple(outer_elements) = &arms.pattern else {
//         panic!("expected tuple pattern");
//     };
//     assert_eq!(outer_elements.len(), 2);

//     match (&outer_elements, &outer_elements) {
//         (
//             TypedPattern::Binding {
//                 data_type: first_type,
//                 ..
//             },
//             TypedPattern::Tuple(inner_elements),
//         ) => {
//             assert_eq!(*first_type, DataType::I32);
//             assert_eq!(inner_elements.len(), 2);
//             for element in inner_elements {
//                 match element {
//                     TypedPattern::Binding { data_type, .. } => {
//                         assert_eq!(*data_type, DataType::I32);
//                     }
//                     other => panic!("unexpected inner pattern: {:?}", other),
//                 }
//             }
//         }
//         other => panic!("unexpected outer patterns: {:?}", other),
//     }
// }

#[test]
fn non_exhaustive_enum_match_is_rejected() {
    let source = r#"
enum Toggle {
    On,
    Off,
}

fn choose |val: Toggle|->i32 {
    match val {
        Toggle::On => 1,
    }
};
"#;

    let err = match analyze_source(source, true) {
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
fn to_int |flag: bool|->i32 {
    match flag {
        true => 1,
    }
};
"#;

    let err = match analyze_source(source, true) {
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

fn find_function_body<'a>(nodes: &'a [TypedAstNode], target: &str) -> &'a TypedExpr {
    nodes
        .iter()
        .find_map(|node| match node {
            TypedAstNode::FnDef { name, body, .. } if name == target => Some(body),
            _ => None,
        })
        .unwrap_or_else(|| panic!("function '{}' not found", target))
}

fn expect_block<'a>(expr: &'a TypedExpr) -> &'a [TypedExpr] {
    match &expr.kind {
        TypedExprKind::Block { statements } => statements,
        other => panic!("expected block expression, found {:?}", other),
    }
}

fn is_i32_literal(expr: &TypedExpr, expected: i32) -> bool {
    matches!(expr.kind, TypedExprKind::Literal(LiteralValue::I32(value)) if value == expected as i64)
}

fn contains_nested_add_pipeline(expr: &TypedExpr) -> bool {
    match &expr.kind {
        TypedExprKind::FunctionCall { name, args } => {
            if name == "add" && args.len() == 2 {
                if is_i32_literal(&args[1], 4)
                    && matches!(
                        &args[0].kind,
                        TypedExprKind::FunctionCall { name: inner_name, args: inner_args }
                            if inner_name == "add"
                                && inner_args.len() == 2
                                && is_i32_literal(&inner_args[0], 1)
                                && matches!(
                                    &inner_args[1].kind,
                                    TypedExprKind::FunctionCall { name: deeper_name, args: deeper_args }
                                        if deeper_name == "add"
                                            && deeper_args.len() == 2
                                            && is_i32_literal(&deeper_args[0], 2)
                                            && is_i32_literal(&deeper_args[1], 3)
                                )
                    )
                {
                    return true;
                }
            }
            args.iter().any(contains_nested_add_pipeline)
        }
        TypedExprKind::Block { statements } => statements.iter().any(contains_nested_add_pipeline),
        _ => false,
    }
}

fn contains_double_chain(expr: &TypedExpr) -> bool {
    match &expr.kind {
        TypedExprKind::FunctionCall { name, args } => {
            if name == "double" && args.len() == 1 {
                if matches!(
                    &args[0].kind,
                    TypedExprKind::FunctionCall { name: inner_name, args: inner_args }
                        if inner_name == "sub"
                            && inner_args.len() == 2
                            && is_i32_literal(&inner_args[0], 10)
                            && is_i32_literal(&inner_args[1], 2)
                ) {
                    return true;
                }
            }
            args.iter().any(contains_double_chain)
        }
        TypedExprKind::Block { statements } => statements.iter().any(contains_double_chain),
        _ => false,
    }
}

#[test]
fn pipeline_extracts_nearest_complete_expression() {
    let source = r#"
fn add |a: i32, b: i32|->i32 $a + b$;

fn compute ||->i32 {
    add 1 add 2 3 > add 4
};

fn main ||->() {};
"#;

    let analysis = analyze_source(source, true).expect("analysis should succeed");
    let body = find_function_body(&analysis.typed_ast, "compute");
    let statements = expect_block(body);
    assert!(
        contains_nested_add_pipeline(&statements[0]),
        "pipeline should inject add(2,3) before final add 4"
    );
}

#[test]
fn pipeline_chains_multiple_functions_in_order() {
    let source = r#"
fn double |n: i32|->i32 $n * 2$;
fn sub |a: i32, b: i32|->i32 $a - b$;

fn chain ||->i32 {
    10 > sub 2 > double
};

fn main ||->() {};
"#;

    let analysis = analyze_source(source, true).expect("analysis should succeed");
    let body = find_function_body(&analysis.typed_ast, "chain");
    let statements = expect_block(body);
    assert!(
        contains_double_chain(&statements[0]),
        "pipeline should thread value into sub before double"
    );
}

#[test]
fn pipeline_requires_function_on_rhs() {
    let source = r#"
fn main ||->i32 {
    10 > 5
};
"#;

    let err = match analyze_source(source, true) {
        Ok(_) => panic!("analysis should fail"),
        Err(err) => err,
    };

    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error, got {:?}", err);
    };
    assert!(
        compile_err
            .message
            .contains("Pipe operator requires a function call"),
        "unexpected error: {}",
        compile_err.message
    );
}

#[test]
fn matches_on_infinite_types_require_wildcard_arm() {
    let source = r#"
fn kind |x: i32|->i32 {
    match x {
        0 => 0,
    }
};
"#;

    let err = match analyze_source(source, true) {
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

#[test]
fn pure_functions_can_call_other_pure_functions() {
    let source = r#"
fn add |lhs: i32, rhs: i32| *> i32 $lhs + rhs$;
fn square |n: i32| *> i32 $n * n$;

fn sum_sq |a: i32, b: i32| *> i32 {
    add square a square b
};

fn main ||->() {
    let result sum_sq 3 4;
    result;
};
"#;

    analyze_source(source, true).expect("pure functions should compose");
}

#[test]
fn pure_function_cannot_call_impure_builtin() {
    let source = r#"
fn loud |text: string| *> string {
    println text;
    text
};
"#;

    let err = match analyze_source(source, true) {
        Ok(_) => panic!("analysis must fail"),
        Err(err) => err,
    };
    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error, got {:?}", err);
    };
    assert!(
        compile_err
            .message
            .contains("Pure functions cannot call impure function 'println'"),
        "unexpected error: {}",
        compile_err.message
    );
}

#[test]
fn pure_function_allows_local_mutation() {
    let source = r#"
fn accumulator |a: i32, b: i32| *> i32 {
    let mut total 0;
    set total $total + a$;
    set total $total + b$;
    total
};
"#;

    analyze_source(source, true).expect("local mutation should be allowed inside pure functions");
}

#[test]
fn generic_function_signature_is_registered() {
    let source = r#"
fn identity<T> |x: T|->T x;
fn main | |->() () ;
"#;

    let analysis = analyze_source(source, true).expect("analysis should succeed");
    let signatures = analysis
        .function_table
        .get("identity")
        .expect("identity should be registered");
    assert_eq!(signatures.len(), 1);
    let signature = &signatures[0];
    assert_eq!(signature.type_params, vec!["T".to_string()]);
    assert_eq!(
        signature.param_types,
        vec![DataType::TypeVar("T".to_string())]
    );
    assert_eq!(signature.return_type, DataType::TypeVar("T".to_string()));
}

#[test]
fn generic_function_calls_infer_argument_types() {
    let source = r#"
fn identity<T> |x: T|->T x;
fn main | |->() {
    let a identity 42;
    let b identity "hello";
    ()
};
"#;

    let analysis = analyze_source(source, true).expect("analysis should succeed");
    let main_body = find_function_body(&analysis.typed_ast, "main");
    let TypedExprKind::Block { statements } = &main_body.kind else {
        panic!("main should be a block");
    };

    let mut call_types = Vec::new();
    for stmt in statements {
        if let TypedExprKind::LetBinding { value, .. } = &stmt.kind {
            if let TypedExprKind::FunctionCall { name, .. } = &value.kind {
                if name == "identity" {
                    call_types.push(value.data_type.clone());
                }
            }
        }
    }

    assert_eq!(call_types, vec![DataType::I32, DataType::String]);
}

#[test]
fn mismatched_generic_arguments_report_error() {
    let source = r#"
fn pick_first<T> |a: T, b: T|->T a;
fn main | |->() {
    pick_first 1 "oops";
};
"#;

    let err = match analyze_source(source, true) {
        Ok(_) => panic!("analysis should fail"),
        Err(err) => err,
    };
    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error");
    };
    assert!(
        compile_err
            .message
            .contains("No overload of 'pick_first' matches argument types"),
        "unexpected error message: {}",
        compile_err.message
    );
    assert!(
        compile_err
            .notes
            .iter()
            .any(|(note, _)| note.contains("type variable 'T'")),
        "expected inference note, got {:?}",
        compile_err.notes
    );
}

#[test]
fn trait_bounds_attach_to_function_signatures() {
    let source = r#"
trait Numeric {
    fn identity |x: Self|->Self;
}

impl Numeric for i32 {
    fn identity |x: Self|->Self x;
}

fn use_numeric<T: Numeric> |value: T|->T value;

fn main | |->() {
    let result use_numeric 42;
    ()
};
"#;

    let analysis = analyze_source(source, true).expect("analysis should succeed");
    let signature = analysis
        .function_table
        .get("use_numeric")
        .and_then(|list| list.first())
        .expect("use_numeric signature present");
    assert_eq!(signature.type_params, vec!["T".to_string()]);
    assert_eq!(signature.trait_bounds.len(), 1);
    let bound = &signature.trait_bounds[0];
    assert_eq!(bound.type_param, "T");
    assert_eq!(bound.trait_name, "Numeric");
    assert!(bound.trait_args.is_empty());

    assert!(analysis.typed_ast.iter().any(
        |node| matches!(node, TypedAstNode::ImplDef { trait_name, .. } if trait_name == "Numeric")
    ));
}

#[test]
fn missing_trait_impl_is_reported() {
    let source = r#"
trait Numeric {
    fn identity |x: Self|->Self;
}

fn use_numeric<T: Numeric> |value: T|->T value;

fn main | |->() {
    let text "missing";
    use_numeric text;
};
"#;

    let err = match analyze_source(source, true) {
        Ok(_) => panic!("analysis should fail"),
        Err(err) => err,
    };
    let LangError::Compile(compile_err) = err else {
        panic!("expected compile error");
    };
    assert!(
        compile_err
            .notes
            .iter()
            .any(|(note, _)| note.contains("does not implement Numeric")),
        "unexpected notes: {:?}",
        compile_err.notes
    );
}

// #[test]
// fn math_block_supports_extended_ops() {
//     let source = r#"
// fn evaluate |a: bool, b: bool, value: i32|->bool {
//     let remainder = $value % 3$;
//     let not_all = $!(a && b)$;
//     let comparison = $remainder == 0$;
//     $(not_all || comparison)$
// };
// "#;

//     let analysis = analyze_source(source, true).expect("analysis should succeed");
//     let function = analysis
//         .typed_ast
//         .iter()
//         .find_map(|node| match node {
//             TypedAstNode::FnDef { name, body, .. } if name == "evaluate" => Some(body),
//             _ => None,
//         })
//         .expect("function definition present");

//     let TypedExprKind::Block { statements } = &function.kind else {
//         panic!("function body should be a block");
//     };
//     assert_eq!(statements.len(), 4);

//     let remainder_binding = &statements;
//     match &remainder_binding.kind {
//         TypedExprKind::LetBinding { value, .. } => {
//             assert_eq!(value.data_type, DataType::I32);
//             if let TypedExprKind::FunctionCall { name, .. } = &value.kind {
//                 assert_eq!(name, "i32.rem_s");
//             } else {
//                 panic!("expected i32.rem_s call");
//             }
//         }
//         other => panic!("unexpected node: {:?}", other),
//     }

//     let not_all_binding = &statements;
//     match &not_all_binding.kind {
//         TypedExprKind::LetBinding { value, .. } => {
//             assert_eq!(value.data_type, DataType::Bool);
//             if let TypedExprKind::FunctionCall { name, .. } = &value.kind {
//                 assert_eq!(name, "i32.eqz");
//             } else {
//                 panic!("expected i32.eqz call");
//             }
//         }
//         other => panic!("unexpected node: {:?}", other),
//     }

//     let comparison_binding = &statements;
//     match &comparison_binding.kind {
//         TypedExprKind::LetBinding { value, .. } => {
//             assert_eq!(value.data_type, DataType::Bool);
//             if let TypedExprKind::FunctionCall { name, .. } = &value.kind {
//                 assert_eq!(name, "i32.eq");
//             } else {
//                 panic!("expected i32.eq call");
//             }
//         }
//         other => panic!("unexpected node: {:?}", other),
//     }

//     let final_expr = statements.last().expect("final expression present");
//     assert_eq!(final_expr.data_type, DataType::Bool);
//     if let TypedExprKind::FunctionCall { name, .. } = &final_expr.kind {
//         assert_eq!(name, "i32.or");
//     } else {
//         panic!("expected logical or call");
//     }
// }

// #[test]
// fn vec_builtins_typecheck() {
//     let source = r#"
// fn use_vec ||->i32 {
//     let mut numbers = vec_new_i32();
//     vec_push_i32(numbers, 10);
//     vec_push_i32(numbers, 20);
//     let second = vec_get_i32(numbers, 1);
//     let len = vec_len_i32(numbers);
//     if $len == 2$ {
//         second
//     } else {
//         0
//     }
// };
// "#;

//     let analysis = analyze_source(source, true).expect("analysis should succeed");
//     let function = analysis
//         .typed_ast
//         .iter()
//         .find_map(|node| match node {
//             TypedAstNode::FnDef { name, body, .. } if name == "use_vec" => Some(body),
//             _ => None,
//         })
//         .expect("function definition present");

//     let TypedExprKind::Block { statements } = &function.kind else {
//         panic!("function body should be a block");
//     };

//     let let_binding = &statements;
//     match &let_binding.kind {
//         TypedExprKind::LetBinding { name: _, value, .. } => {
//             assert_eq!(value.data_type, DataType::Vector(Box::new(DataType::I32)));
//             if let TypedExprKind::FunctionCall { name, .. } = &value.kind {
//                 assert_eq!(name, "vec_new_i32");
//             } else {
//                 panic!("expected vec_new_i32 call");
//             }
//         }
//         other => panic!("unexpected node: {:?}", other),
//     }

//     let len_binding = &statements;
//     match &len_binding.kind {
//         TypedExprKind::LetBinding { value, .. } => {
//             assert_eq!(value.data_type, DataType::I32);
//             if let TypedExprKind::FunctionCall { name, .. } = &value.kind {
//                 assert_eq!(name, "vec_len_i32");
//             } else {
//                 panic!("expected vec_len_i32 call");
//             }
//         }
//         other => panic!("unexpected node: {:?}", other),
//     }
// }

// #[test]
// fn string_char_at_is_typed() {
//     let source = r#"
// fn third |text: string|->string {
//     string_char_at(text, 2)
// };
// "#;

//     let analysis = analyze_source(source, true).expect("analysis should succeed");
//     let function = analysis
//         .typed_ast
//         .iter()
//         .find_map(|node| match node {
//             TypedAstNode::FnDef { name, body, .. } if name == "third" => Some(body),
//             _ => None,
//         })
//         .expect("function definition present");

//     let TypedExprKind::Block { statements } = &function.kind else {
//         panic!("function body should be a block");
//     };
//     let call_expr = statements.last().expect("call expression present");
//     if let TypedExprKind::FunctionCall { name, args } = &call_expr.kind {
//         assert_eq!(name, "string_char_at");
//         assert_eq!(args.len(), 2);
//         assert_eq!(args.data_type, DataType::String);
//         assert_eq!(args.data_type, DataType::I32);
//     } else {
//         panic!("expected direct call expression");
//     }
//     assert_eq!(call_expr.data_type, DataType::String);
// }
