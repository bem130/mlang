use mylang_core::analyze_source;
use mylang_wasm_codegen::WasmGenerator;
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};
use wasmi::{Caller, Engine, Linker, Module, Store, core::Trap};

#[derive(Debug, Deserialize)]
struct FunctionMetadata {
    #[allow(dead_code)]
    name: String,
    #[allow(dead_code)]
    #[serde(default)]
    params: Vec<String>,
    #[allow(dead_code)]
    return_type: String,
}

#[derive(Debug, Deserialize)]
#[allow(dead_code)]
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
        #[allow(dead_code)]
        functions: Vec<FunctionMetadata>,
        #[serde(default)]
        stdin: String,
        #[serde(default)]
        stdout: Vec<String>,
    },
    Error {
        #[allow(dead_code)]
        error: ErrorMetadata,
        #[serde(default)]
        #[allow(dead_code)]
        notes_contains: Vec<String>,
    },
}

struct SampleCase {
    name: String,
    source: String,
    metadata: SampleMetadata,
}

struct TestWasiCtx {
    stdout: Vec<u8>,
    stdin: std::io::Cursor<Vec<u8>>,
}

#[test]
fn passing_samples_work_as_expected() {
    for sample in load_samples("passing") {
        let (expected_stdout, stdin_data) = match &sample.metadata {
            SampleMetadata::Ok { stdout, stdin, .. } => (stdout.clone(), stdin.clone()),
            SampleMetadata::Error { .. } => panic!(
                "sample {} is marked as error metadata but located in passing directory",
                sample.name
            ),
        };

        let analysis = analyze_source(&sample.source)
            .unwrap_or_else(|err| panic!("expected Ok for {}, got Err: {}", sample.name, err));

        let mylang_core::AnalysisResult {
            typed_ast,
            function_table,
            string_headers,
            static_offset,
        } = analysis;

        let mut generator = WasmGenerator::new(function_table, string_headers, static_offset);
        let wat = generator
            .generate(&typed_ast)
            .unwrap_or_else(|err| panic!("failed to generate WAT for {}: {}", sample.name, err));

        let stdout = run_wasm(&wat, stdin_data.as_bytes())
            .unwrap_or_else(|err| panic!("failed to execute {}: {}", sample.name, err));

        let actual_lines: Vec<String> = stdout.lines().map(|line| line.to_string()).collect();

        assert_eq!(
            actual_lines, expected_stdout,
            "stdout mismatch for sample {}",
            sample.name
        );
    }
}

fn run_wasm(wat: &str, stdin: &[u8]) -> Result<String, Box<dyn std::error::Error>> {
    let engine = Engine::default();
    let mut store = Store::new(
        &engine,
        TestWasiCtx {
            stdout: Vec::new(),
            stdin: std::io::Cursor::new(stdin.to_vec()),
        },
    );
    let mut linker = Linker::new(&engine);

    linker.func_wrap(
        "wasi_snapshot_preview1",
        "fd_write",
        |mut caller: Caller<'_, TestWasiCtx>,
         fd: i32,
         iovecs_ptr: i32,
         iovecs_len: i32,
         nwritten_ptr: i32|
         -> Result<i32, Trap> {
            if fd != 1 && fd != 2 {
                const ERRNO_NOTSUP: i32 = 58;
                return Ok(ERRNO_NOTSUP);
            }

            let memory = caller
                .get_export("memory")
                .and_then(|ext| ext.into_memory())
                .ok_or_else(|| Trap::new("failed to find memory export"))?;

            let mut written_bytes: u32 = 0;
            let iovecs_ptr = iovecs_ptr as u32;

            for i in 0..iovecs_len {
                let iovec_offset = (iovecs_ptr + (i as u32) * 8) as usize;
                let mut buf_ptr_bytes = [0u8; 4];
                memory
                    .read(&caller, iovec_offset, &mut buf_ptr_bytes)
                    .map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_ptr = u32::from_le_bytes(buf_ptr_bytes);

                let mut buf_len_bytes = [0u8; 4];
                memory
                    .read(&caller, iovec_offset + 4, &mut buf_len_bytes)
                    .map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_len = u32::from_le_bytes(buf_len_bytes);

                let mut buffer = vec![0u8; buf_len as usize];
                memory
                    .read(&caller, buf_ptr as usize, &mut buffer)
                    .map_err(|_| Trap::new("buffer out of bounds"))?;

                caller.data_mut().stdout.extend_from_slice(&buffer);
                written_bytes += buf_len;
            }

            memory
                .write(
                    &mut caller,
                    nwritten_ptr as usize,
                    &written_bytes.to_le_bytes(),
                )
                .map_err(|_| Trap::new("pointer out of bounds"))?;

            const ERRNO_SUCCESS: i32 = 0;
            Ok(ERRNO_SUCCESS)
        },
    )?;

    linker.func_wrap(
        "wasi_snapshot_preview1",
        "fd_read",
        |mut caller: Caller<'_, TestWasiCtx>,
         fd: i32,
         iovecs_ptr: i32,
         iovecs_len: i32,
         nread_ptr: i32|
         -> Result<i32, Trap> {
            use std::io::Read;

            if fd != 0 {
                // Not stdin
                const ERRNO_BADF: i32 = 8;
                return Ok(ERRNO_BADF);
            }

            let memory = caller
                .get_export("memory")
                .and_then(|ext| ext.into_memory())
                .ok_or_else(|| Trap::new("failed to find memory export"))?;

            let mut read_bytes: u32 = 0;
            let iovecs_ptr = iovecs_ptr as u32;

            for i in 0..iovecs_len {
                let iovec_offset = (iovecs_ptr + (i as u32) * 8) as usize;
                let mut buf_ptr_bytes = [0u8; 4];
                memory
                    .read(&caller, iovec_offset, &mut buf_ptr_bytes)
                    .map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_ptr = u32::from_le_bytes(buf_ptr_bytes);

                let mut buf_len_bytes = [0u8; 4];
                memory
                    .read(&caller, iovec_offset + 4, &mut buf_len_bytes)
                    .map_err(|_| Trap::new("pointer out of bounds"))?;
                let buf_len = u32::from_le_bytes(buf_len_bytes);

                let mut buffer = vec![0u8; buf_len as usize];
                let n = caller
                    .data_mut()
                    .stdin
                    .read(&mut buffer)
                    .map_err(|_| Trap::new("failed to read from stdin"))?;

                let buffer = &buffer[..n];
                memory
                    .write(&mut caller, buf_ptr as usize, buffer)
                    .map_err(|_| Trap::new("buffer out of bounds"))?;

                read_bytes += n as u32;
            }

            memory
                .write(
                    &mut caller,
                    nread_ptr as usize,
                    &read_bytes.to_le_bytes(),
                )
                .map_err(|_| Trap::new("pointer out of bounds"))?;

            const ERRNO_SUCCESS: i32 = 0;
            Ok(ERRNO_SUCCESS)
        },
    )?;

    let wasm_binary = wat::parse_str(wat)?;
    let module = Module::new(&engine, wasm_binary.as_slice())?;

    let instance = linker.instantiate(&mut store, &module)?.start(&mut store)?;

    let start_func = instance
        .get_func(&store, "_start")
        .ok_or_else(|| "Wasm module missing '_start' export")?;

    start_func.call(&mut store, &[], &mut [])?;

    let stdout = String::from_utf8(store.data().stdout.clone())?;
    Ok(stdout)
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
fn nested_tuple_patterns_execute_correctly() {
    let source = r#"
fn main() {
    let total = match (1, (2, 3)) {
        (a, (b, c)) => $a + b + c$,
    };
    println(i32_to_string(total));
}
"#;

    let analysis = analyze_source(source).expect("analysis should succeed");
    let mylang_core::AnalysisResult {
        typed_ast,
        function_table,
        string_headers,
        static_offset,
    } = analysis;

    let mut generator = WasmGenerator::new(function_table, string_headers, static_offset);
    let wat = generator
        .generate(&typed_ast)
        .expect("code generation should succeed");

    let stdout = run_wasm(&wat, b"").expect("executing Wasm module should succeed");

    assert_eq!(stdout.trim(), "6");
}

fn samples_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("tests")
        .join("samples")
}
