use mylang_core::analyze_source;
use mylang_core::ast::{DataType, FunctionPurity, TypedAstNode};
use mylang_core::compiler::FunctionSignature;
use mylang_core::error::LangError;
use serde::Deserialize;
use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::panic::{self, AssertUnwindSafe};
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

#[derive(Debug)]
#[allow(dead_code)]
struct MultiFileSampleCase {
    name: String,
    root: PathBuf,
    entry: PathBuf,
    files: Vec<(PathBuf, String)>,
    metadata: SampleMetadata,
}

#[derive(Debug, Deserialize)]
struct MultiFileMetadata {
    entry: String,
    #[serde(flatten)]
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
            let entry = entry.unwrap_or_else(|err| {
                panic!("failed to read directory entry in {:?}: {}", dir, err)
            });
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

fn multi_file_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("tests")
        .join("multi_file")
}

fn load_multi_file_samples(subdir: &str) -> Vec<MultiFileSampleCase> {
    let mut dir = multi_file_root();
    dir.push(subdir);

    let mut entries: Vec<_> = fs::read_dir(&dir)
        .unwrap_or_else(|err| panic!("failed to read {:?}: {}", dir, err))
        .filter_map(|entry| {
            let entry = entry.unwrap_or_else(|err| {
                panic!("failed to read directory entry in {:?}: {}", dir, err)
            });
            let path = entry.path();
            if path.is_dir() { Some(path) } else { None }
        })
        .collect();
    entries.sort();

    entries
        .into_iter()
        .map(|path| {
            let metadata_path = path.join("case.meta.json");
            let metadata_str = fs::read_to_string(&metadata_path).unwrap_or_else(|err| {
                panic!(
                    "failed to read metadata for {:?}: {}",
                    path.file_name().unwrap(),
                    err
                )
            });
            let metadata: MultiFileMetadata =
                serde_json::from_str(&metadata_str).unwrap_or_else(|err| {
                    panic!(
                        "failed to parse metadata JSON for {:?}: {}",
                        metadata_path, err
                    )
                });

            let files = collect_mlang_sources(&path);

            MultiFileSampleCase {
                name: path
                    .file_name()
                    .map(|os| os.to_string_lossy().into_owned())
                    .unwrap_or_else(|| path.display().to_string()),
                root: path,
                entry: PathBuf::from(metadata.entry),
                files,
                metadata: metadata.metadata,
            }
        })
        .collect()
}

fn collect_mlang_sources(root: &Path) -> Vec<(PathBuf, String)> {
    let mut stack = vec![root.to_path_buf()];
    let mut files = Vec::new();

    while let Some(dir) = stack.pop() {
        for entry in fs::read_dir(&dir)
            .unwrap_or_else(|err| panic!("failed to read directory {:?}: {}", dir, err))
        {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().map_or(false, |ext| ext == "mlang") {
                let rel_path = path
                    .strip_prefix(root)
                    .unwrap_or_else(|_| panic!("failed to strip prefix for {:?}", path))
                    .to_path_buf();
                let source = fs::read_to_string(&path)
                    .unwrap_or_else(|err| panic!("failed to read {:?}: {}", path, err));
                files.push((rel_path, source));
            }
        }
    }

    files.sort_by(|a, b| a.0.cmp(&b.0));
    files
}

#[test]
fn passing_samples_match_metadata() {
    let mut failures = Vec::new();
    for sample in load_samples("passing") {
        let result = panic::catch_unwind(AssertUnwindSafe(|| match sample.metadata {
            SampleMetadata::Ok { functions, .. } => {
                let analysis = analyze_source(&sample.source, false).unwrap_or_else(|err| {
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

                // If the metadata explicitly lists functions, assert the count.
                // If not provided (empty), skip the strict count check to allow
                // both script-style samples and explicit `fn` definitions.
                if !functions.is_empty() {
                    assert_eq!(
                        function_nodes.len(),
                        functions.len(),
                        "typed AST function count mismatch for {}",
                        sample.name
                    );
                }

                let mut typed_functions_by_name: HashMap<String, Vec<(Vec<DataType>, DataType)>> =
                    HashMap::new();
                for node in &function_nodes {
                    if let TypedAstNode::FnDef {
                        name,
                        params,
                        return_type,
                        ..
                    } = node
                    {
                        let param_types =
                            params.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>();
                        typed_functions_by_name
                            .entry(name.clone())
                            .or_default()
                            .push((param_types, return_type.clone()));
                    }
                }

                for func_meta in &functions {
                    let entries = typed_functions_by_name
                        .get_mut(&func_meta.name)
                        .unwrap_or_else(|| {
                            panic!(
                                "function '{}' not found in typed AST for {}",
                                func_meta.name, sample.name
                            )
                        });
                    let match_index = entries
                        .iter()
                        .position(|(params, ret)| {
                            let param_strings: Vec<String> =
                                params.iter().map(data_type_to_str).collect();
                            param_strings == func_meta.params
                                && data_type_to_str(ret) == func_meta.return_type
                        })
                        .unwrap_or_else(|| {
                            let available: Vec<String> = entries
                                .iter()
                                .map(|(params, ret)| {
                                    let param_repr = params
                                        .iter()
                                        .map(data_type_to_str)
                                        .collect::<Vec<_>>()
                                        .join(", ");
                                    format!("({}) -> {}", param_repr, data_type_to_str(ret))
                                })
                                .collect();
                            panic!(
                                "function '{}' with signature {:?} -> {} not found in typed AST for {}. Available overloads: {:?}",
                                func_meta.name,
                                func_meta.params,
                                func_meta.return_type,
                                sample.name,
                                available
                            );
                        });
                    let (param_types, return_type) = entries.remove(match_index);
                    let rendered_params: Vec<String> =
                        param_types.iter().map(data_type_to_str).collect();
                    assert_eq!(
                        rendered_params, func_meta.params,
                        "parameter type mismatch for function '{}' in {}",
                        func_meta.name, sample.name
                    );
                    assert_eq!(
                        data_type_to_str(&return_type),
                        func_meta.return_type,
                        "return type mismatch for function '{}' in {}",
                        func_meta.name,
                        sample.name
                    );

                    let signatures =
                        analysis
                            .function_table
                            .get(&func_meta.name)
                            .unwrap_or_else(|| {
                                panic!(
                                    "function '{}' missing from function_table for {}",
                                    func_meta.name, sample.name
                                )
                            });

                    assert_function_signature_matches(signatures, func_meta, &sample.name);
                }
            }
            SampleMetadata::Error { .. } => panic!(
                "sample {} is marked as error metadata but located in passing directory",
                sample.name
            ),
        }));

        if let Err(payload) = result {
            failures.push(format!(
                "{}: {}",
                sample.name,
                format_panic_payload(&payload)
            ));
        }
    }

    if !failures.is_empty() {
        panic!(
            "{} passing sample(s) failed:\n{}",
            failures.len(),
            failures.join("\n\n")
        );
    }
}

#[test]
fn failing_samples_match_metadata() {
    let mut failures = Vec::new();
    for sample in load_samples("failing") {
        let result = panic::catch_unwind(AssertUnwindSafe(|| {
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

            let err = match analyze_source(&sample.source, false) {
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
        }));

        if let Err(payload) = result {
            failures.push(format!(
                "{}: {}",
                sample.name,
                format_panic_payload(&payload)
            ));
        }
    }

    if !failures.is_empty() {
        panic!(
            "{} failing sample(s) failed:\n{}",
            failures.len(),
            failures.join("\n\n")
        );
    }
}

#[test]
fn multi_file_sample_layout_is_consistent() {
    let passing = load_multi_file_samples("passing");
    let failing = load_multi_file_samples("failing");

    assert!(
        !passing.is_empty(),
        "expected at least one multi-file passing sample"
    );
    assert!(
        !failing.is_empty(),
        "expected at least one multi-file failing sample"
    );

    for sample in passing.into_iter().chain(failing.into_iter()) {
        assert!(
            !sample.entry.is_absolute(),
            "entry path must be relative for {}",
            sample.name
        );

        assert!(
            !sample.files.is_empty(),
            "no source files collected for multi-file sample {}",
            sample.name
        );

        let mut seen_paths: HashSet<&Path> = HashSet::new();
        let mut has_entry = false;
        for (rel_path, _) in &sample.files {
            has_entry |= rel_path == &sample.entry;
            assert!(
                seen_paths.insert(rel_path.as_path()),
                "duplicate source {:?} found while loading sample {}",
                rel_path,
                sample.name
            );
        }

        assert!(
            has_entry,
            "entry file {:?} missing from multi-file sample {}",
            sample.entry, sample.name
        );
    }
}

#[test]
#[ignore = "multi-file include/import analysis not implemented yet"]
fn multi_file_samples_match_metadata() {
    let passing = load_multi_file_samples("passing");
    let failing = load_multi_file_samples("failing");

    assert!(
        !passing.is_empty() || !failing.is_empty(),
        "expected multi-file samples to be present"
    );

    todo!("integrate multi-file analysis pipeline");
}

fn assert_function_signature_matches(
    signatures: &[FunctionSignature],
    metadata: &FunctionMetadata,
    sample_name: &str,
) {
    if let Some(signature) = signatures.iter().find(|sig| {
        let params: Vec<String> = sig.param_types.iter().map(data_type_to_str).collect();
        params == metadata.params && data_type_to_str(&sig.return_type) == metadata.return_type
    }) {
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
        return;
    }

    let available: Vec<String> = signatures
        .iter()
        .map(|sig| {
            let params = sig
                .param_types
                .iter()
                .map(data_type_to_str)
                .collect::<Vec<_>>()
                .join(", ");
            format!("fn {}({}) -> {}", sig.name, params, sig.return_type)
        })
        .collect();
    panic!(
        "expected to find function '{}' with params {:?} and return {} in {}. Available overloads: {:?}",
        metadata.name, metadata.params, metadata.return_type, sample_name, available
    );
}

fn data_type_to_str(data_type: &DataType) -> String {
    match data_type {
        DataType::I32 => "i32".to_string(),
        DataType::F64 => "f64".to_string(),
        DataType::Bool => "bool".to_string(),
        DataType::String => "string".to_string(),
        DataType::TypeVar(name) => name.clone(),
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
        DataType::Function {
            params,
            return_type,
            purity,
        } => {
            let params_s = params
                .iter()
                .map(data_type_to_str)
                .collect::<Vec<_>>()
                .join(", ");
            let arrow = match purity {
                FunctionPurity::Pure => "*>",
                FunctionPurity::Impure => "->",
            };
            format!("({}) {} {}", params_s, arrow, data_type_to_str(return_type))
        }
        DataType::Refined { base, .. } => data_type_to_str(base),
    }
}

fn format_panic_payload(payload: &(dyn Any + Send)) -> String {
    if let Some(msg) = payload.downcast_ref::<&str>() {
        msg.to_string()
    } else if let Some(msg) = payload.downcast_ref::<String>() {
        msg.clone()
    } else {
        "non-string panic payload".to_string()
    }
}
