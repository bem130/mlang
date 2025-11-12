use mylang_core::analyze_source;
use mylang_core::ast::{DataType, TypedAstNode};
use mylang_core::compiler::FunctionSignature;
use mylang_core::error::LangError;
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Deserialize)]
struct FunctionMetadata {
    name: String,
    #[serde(default)]
    params: Vec<String>,
    return_type: String,
}

#[derive(Debug, Deserialize)]
struct ErrorMetadata {
    variant: ErrorVariant,
    message_contains: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
enum ErrorVariant {
    Parse,
    Compile,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "expect", rename_all = "lowercase")]
enum SampleMetadata {
    Ok {
        #[serde(default)]
        functions: Vec<FunctionMetadata>,
        #[serde(default)]
        #[allow(dead_code)]
        stdout: Vec<String>,
    },
    Error {
        error: ErrorMetadata,
        #[serde(default)]
        notes_contains: Vec<String>,
    },
}

struct SampleCase {
    name: String,
    source: String,
    metadata: SampleMetadata,
}

fn samples_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("tests")
        .join("samples")
}

fn load_samples(subdir: &str) -> Vec<SampleCase> {
    let mut dir = samples_root();
    dir.push(subdir);

    let mut entries: Vec<_> = fs::read_dir(&dir)
        .unwrap_or_else(|err| panic!("failed to read {:?}: {}", dir, err))
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "mlang") {
                Some(path)
            } else {
                None
            }
        })
        .collect();
    entries.sort();

    entries
        .into_iter()
        .map(|path| {
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("failed to read {:?}: {}", path, err));
            let metadata_path = path.with_extension("mlang.meta.json");
            let metadata_str = fs::read_to_string(&metadata_path).unwrap_or_else(|err| {
                panic!(
                    "failed to read metadata for {:?}: {}",
                    path.file_name().unwrap(),
                    err
                )
            });
            let metadata: SampleMetadata =
                serde_json::from_str(&metadata_str).unwrap_or_else(|err| {
                    panic!(
                        "failed to parse metadata JSON for {:?}: {}",
                        metadata_path, err
                    )
                });
            SampleCase {
                name: path
                    .file_name()
                    .map(|os| os.to_string_lossy().into_owned())
                    .unwrap_or_else(|| path.display().to_string()),
                source,
                metadata,
            }
        })
        .collect()
}

#[test]
fn passing_samples_match_metadata() {
    for sample in load_samples("passing") {
        match sample.metadata {
            SampleMetadata::Ok { functions, .. } => {
                let analysis = analyze_source(&sample.source).unwrap_or_else(|err| {
                    panic!("expected Ok for {}, got Err: {}", sample.name, err)
                });

                let function_nodes: Vec<_> = analysis
                    .typed_ast
                    .iter()
                    .filter_map(|node| match node {
                        TypedAstNode::FnDef { .. } => Some(node),
                        _ => None,
                    })
                    .collect();

                assert_eq!(
                    function_nodes.len(),
                    functions.len(),
                    "typed AST function count mismatch for {}",
                    sample.name
                );

                for func_meta in &functions {
                    let typed_node = function_nodes
                        .iter()
                        .find_map(|node| match node {
                            TypedAstNode::FnDef {
                                name,
                                params,
                                return_type,
                                ..
                            } if name == &func_meta.name => Some((params, return_type)),
                            _ => None,
                        })
                        .unwrap_or_else(|| {
                            panic!(
                                "function '{}' not found in typed AST for {}",
                                func_meta.name, sample.name
                            )
                        });

                    let param_types: Vec<String> = typed_node
                        .0
                        .iter()
                        .map(|(_, ty)| data_type_to_str(ty))
                        .collect();
                    assert_eq!(
                        param_types, func_meta.params,
                        "parameter type mismatch for function '{}' in {}",
                        func_meta.name, sample.name
                    );

                    assert_eq!(
                        data_type_to_str(typed_node.1),
                        func_meta.return_type,
                        "return type mismatch for function '{}' in {}",
                        func_meta.name,
                        sample.name
                    );

                    let signature =
                        analysis
                            .function_table
                            .get(&func_meta.name)
                            .unwrap_or_else(|| {
                                panic!(
                                    "function '{}' missing from function_table for {}",
                                    func_meta.name, sample.name
                                )
                            });

                    assert_function_signature_matches(signature, func_meta, &sample.name);
                }
            }
            SampleMetadata::Error { .. } => panic!(
                "sample {} is marked as error metadata but located in passing directory",
                sample.name
            ),
        }
    }
}

#[test]
fn failing_samples_match_metadata() {
    for sample in load_samples("failing") {
        let (error_meta, notes_expect) = match sample.metadata {
            SampleMetadata::Error {
                error,
                notes_contains,
            } => (error, notes_contains),
            SampleMetadata::Ok { .. } => panic!(
                "sample {} is marked as ok metadata but located in failing directory",
                sample.name
            ),
        };

        let err = match analyze_source(&sample.source) {
            Ok(_) => panic!("expected Err for {}", sample.name),
            Err(err) => err,
        };

        match (&err, error_meta.variant) {
            (LangError::Parse(_), ErrorVariant::Parse) => {}
            (LangError::Compile(_), ErrorVariant::Compile) => {}
            (actual, expected) => panic!(
                "error variant mismatch for {}: expected {:?}, got {:?}",
                sample.name, expected, actual
            ),
        }

        let message = err.to_string();
        assert!(
            message.contains(&error_meta.message_contains),
            "error message for {} did not contain {:?}: {}",
            sample.name,
            error_meta.message_contains,
            message
        );

        if !notes_expect.is_empty() {
            let compile_err = match &err {
                LangError::Compile(compile_err) => compile_err,
                _ => panic!(
                    "notes expectations provided for non-compile error in {}",
                    sample.name
                ),
            };

            for expected in &notes_expect {
                let matched = compile_err
                    .notes
                    .iter()
                    .any(|(note, _)| note.contains(expected));
                assert!(
                    matched,
                    "expected note containing {:?} not found for {}. Notes: {:?}",
                    expected, sample.name, compile_err.notes
                );
            }
        }
    }
}

fn assert_function_signature_matches(
    signature: &FunctionSignature,
    metadata: &FunctionMetadata,
    sample_name: &str,
) {
    let param_types: Vec<String> = signature.param_types.iter().map(data_type_to_str).collect();
    assert_eq!(
        param_types, metadata.params,
        "function_table parameter mismatch for function '{}' in {}",
        metadata.name, sample_name
    );

    assert_eq!(
        data_type_to_str(&signature.return_type),
        metadata.return_type,
        "function_table return type mismatch for function '{}' in {}",
        metadata.name,
        sample_name
    );
}

fn data_type_to_str(data_type: &DataType) -> String {
    match data_type {
        DataType::I32 => "i32".to_string(),
        DataType::F64 => "f64".to_string(),
        DataType::Bool => "bool".to_string(),
        DataType::String => "string".to_string(),
        DataType::Vector(inner) => format!("Vec<{}>", data_type_to_str(inner)),
        DataType::Unit => "unit".to_string(),
        DataType::Tuple(elements) => {
            let inner = elements
                .iter()
                .map(data_type_to_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        DataType::Struct(name) | DataType::Enum(name) => name.clone(),
    }
}
