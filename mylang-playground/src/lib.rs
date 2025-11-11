use mylang_core::analyze_source;
use mylang_wasm_codegen::WasmGenerator;
use wasm_bindgen::prelude::*;
use wasmi::{core::Trap, Caller, Engine, Linker, Module, Store};

struct HostState {
    stdout: Vec<u8>,
}

impl Default for HostState {
    fn default() -> Self {
        Self { stdout: Vec::new() }
    }
}

#[wasm_bindgen]
pub struct RunResult {
    wat: String,
    stdout: String,
}

#[wasm_bindgen]
impl RunResult {
    #[wasm_bindgen(getter)]
    pub fn wat(&self) -> String {
        self.wat.clone()
    }

    #[wasm_bindgen(getter)]
    pub fn stdout(&self) -> String {
        self.stdout.clone()
    }
}

#[wasm_bindgen]
pub fn compile_to_wat(source: &str) -> Result<String, JsValue> {
    generate_wat(source).map_err(|e| JsValue::from_str(&e))
}

#[wasm_bindgen]
pub fn run(source: &str) -> Result<RunResult, JsValue> {
    let wat = generate_wat(source).map_err(|e| JsValue::from_str(&e))?;
    let stdout = execute_wat(&wat).map_err(|e| JsValue::from_str(&e))?;

    Ok(RunResult { wat, stdout })
}

fn generate_wat(source: &str) -> Result<String, String> {
    let analysis_result = analyze_source(source.trim()).map_err(|e| e.to_string())?;

    let mut wasm_generator = WasmGenerator::new(
        analysis_result.function_table,
        analysis_result.string_headers,
        analysis_result.static_offset,
    );

    wasm_generator
        .generate(&analysis_result.typed_ast)
        .map_err(|e| e.to_string())
}

fn execute_wat(wat_code: &str) -> Result<String, String> {
    let engine = Engine::default();
    let mut store = Store::new(&engine, HostState::default());
    let mut linker = <Linker<HostState>>::new(&engine);

    linker
        .func_wrap(
            "wasi_snapshot_preview1",
            "fd_write",
            |mut caller: Caller<'_, HostState>,
             fd: i32,
             iovecs_ptr: i32,
             iovecs_len: i32,
             nwritten_ptr: i32|
             -> Result<i32, Trap> {
                let memory = caller
                    .get_export("memory")
                    .and_then(|ext| ext.into_memory())
                    .ok_or_else(|| Trap::new("failed to find memory export"))?;

                if fd != 1 && fd != 2 {
                    const ERRNO_NOTSUP: i32 = 58;
                    return Ok(ERRNO_NOTSUP);
                }

                let mut written_bytes: u32 = 0;
                let base_ptr = iovecs_ptr as u32;

                for i in 0..iovecs_len {
                    let offset = (base_ptr + (i as u32 * 8)) as usize;

                    let mut buf_ptr_bytes = [0u8; 4];
                    memory
                        .read(&caller, offset, &mut buf_ptr_bytes)
                        .map_err(|_| Trap::new("pointer out of bounds"))?;
                    let buf_ptr = u32::from_le_bytes(buf_ptr_bytes);

                    let mut buf_len_bytes = [0u8; 4];
                    memory
                        .read(&caller, offset + 4, &mut buf_len_bytes)
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
        )
        .map_err(|e| e.to_string())?;

    let wasm_binary = wat::parse_str(wat_code).map_err(|e| e.to_string())?;
    let module = Module::new(&engine, &*wasm_binary).map_err(|e| e.to_string())?;

    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|e| e.to_string())?
        .start(&mut store)
        .map_err(|e| e.to_string())?;

    let start_func = instance
        .get_func(&store, "_start")
        .ok_or_else(|| "Wasmモジュールに関数 '_start' が見つかりません".to_string())?;

    start_func
        .call(&mut store, &[], &mut [])
        .map_err(|e| e.to_string())?;

    let output = String::from_utf8_lossy(&store.data().stdout).into_owned();

    Ok(output)
}
