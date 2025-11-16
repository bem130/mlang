use wasm_bindgen::prelude::*;
use wasmi::{
    Caller, Config, Engine, Linker, Module, Store, Func, TrapCode, Error as WasmError,
};
use std::collections::VecDeque;
use mylang_core::analyze_source;
use mylang_wasm_codegen::WasmGenerator;

const AWAIT_STDIN_TRAP_MSG: &str = "Awaiting Stdin";

struct HostState {
    stdout: Vec<u8>,
    stdin: VecDeque<u8>,
}

impl Default for HostState {
    fn default() -> Self {
        Self {
            stdout: Vec::new(),
            stdin: VecDeque::new(),
        }
    }
}

#[wasm_bindgen]
pub struct StepRunner {
    engine: Engine,
    store: Store<HostState>,
    start_func: Func,
    finished: bool,
}

#[wasm_bindgen]
pub fn compile_to_wat(source: &str) -> Result<String, JsValue> {
    generate_wat(source).map_err(|e| JsValue::from_str(&e))
}

#[wasm_bindgen]
impl StepRunner {
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str) -> Result<StepRunner, JsValue> {
        // Configure the engine to enable fuel metering.
        let mut config = Config::default();
        config.consume_fuel(true);
        let engine = Engine::new(&config);

        // Compile the source into a Wasm module.
        let wat = generate_wat(source).map_err(js_error)?;
        let wasm_binary = wat::parse_str(&wat).map_err(js_error)?;
        let module = Module::new(&engine, &*wasm_binary).map_err(js_error)?;

        // Instantiate the module and look up the `_start` function.
        let (store, start_func) =
            Self::prepare_execution(&engine, &module).map_err(js_error)?;

        Ok(StepRunner {
            engine,
            store,
            start_func,
            finished: false,
        })
    }

    pub fn stdout(&mut self) -> String {
        let output = String::from_utf8_lossy(&self.store.data().stdout).to_string();
        self.store.data_mut().stdout.clear();
        output
    }

    pub fn is_complete(&self) -> bool {
        self.finished
    }

    pub fn step(&mut self, fuel: u64) -> Result<i32, JsValue> {
        // If the program already finished, return Completed immediately.
        if self.finished {
            return Ok(StepStatus::Completed as i32);
        }

        // Set the amount of fuel available for this step.
        self.store
            .set_fuel(fuel)
            .map_err(|err| js_error(err.to_string()))?;

        // Call the `_start` function. We interpret specific traps
        // as either "awaiting input" or "out of fuel".
        match self.start_func.call(&mut self.store, &[], &mut []) {
            Ok(()) => {
                // Program finished successfully.
                self.finished = true;
                Ok(StepStatus::Completed as i32)
            }
            Err(err) => {
                let msg = err.to_string();
                if msg.contains(AWAIT_STDIN_TRAP_MSG) {
                    // Waiting for stdin from the UI.
                    Ok(StepStatus::AwaitingInput as i32)
                } else if let Some(TrapCode::OutOfFuel) = err.as_trap_code() {
                    // Execution ran out of fuel; allow another step.
                    Ok(StepStatus::InProgress as i32)
                } else {
                    // Any other error is surfaced to JS.
                    self.finished = true;
                    Err(js_error(msg))
                }
            }
        }
    }

    pub fn provide_stdin(&mut self, input: &str) {
        self.store.data_mut().stdin.extend(input.as_bytes());
    }

    pub fn reset(&mut self) -> Result<(), JsValue> {
        self.finished = true;
        Err(js_error("Reset requires creating a new StepRunner instance."))
    }

    fn prepare_execution(
        engine: &Engine,
        module: &Module,
    ) -> Result<(Store<HostState>, Func), String> {
        let mut store = Store::new(engine, HostState::default());
        let mut linker = Linker::<HostState>::new(engine);

        link_wasi_functions(&mut linker)?;

        let instance = linker
            .instantiate(&mut store, module)
            .map_err(|e| e.to_string())?
            .start(&mut store)
            .map_err(|e| e.to_string())?;

        let start_func = instance
            .get_func(&store, "_start")
            .ok_or_else(|| "Wasmモジュールに関数 '_start' が見つかりません".to_string())?;

        Ok((store, start_func))
    }
}

fn generate_wat(source: &str) -> Result<String, String> {
    let analysis_result = analyze_source(source.trim(), false).map_err(|e| e.to_string())?;

    let mut wasm_generator = WasmGenerator::new(
        analysis_result.function_table,
        analysis_result.string_headers,
        analysis_result.static_offset,
    );

    wasm_generator
        .generate(&analysis_result.typed_ast)
        .map_err(|e| e.to_string())
}

/// Link a minimal subset of WASI needed by the playground.
fn link_wasi_functions(linker: &mut Linker<HostState>) -> Result<(), String> {
    // Minimal WASI fd_write implementation that writes to HostState.stdout.
    linker
        .func_wrap(
            "wasi_snapshot_preview1",
            "fd_write",
            |mut caller: Caller<'_, HostState>,
             fd: i32,
             iovecs_ptr: i32,
             iovecs_len: i32,
             nwritten_ptr: i32|
             -> Result<i32, WasmError> {
                const ERRNO_SUCCESS: i32 = 0;
                const ERRNO_BADF: i32 = 8;

                // Only stdout (1) and stderr (2) are supported.
                if fd != 1 && fd != 2 {
                    return Ok(ERRNO_BADF);
                }

                let memory = caller
                    .get_export("memory")
                    .and_then(|ext| ext.into_memory())
                    .ok_or_else(|| WasmError::new("failed to find memory export"))?;

                let mut total_written: u32 = 0;
                let mut offset = iovecs_ptr as usize;

                for _ in 0..iovecs_len {
                    let mut buf_ptr_bytes = [0u8; 4];
                    let mut buf_len_bytes = [0u8; 4];

                    memory.read(&caller, offset, &mut buf_ptr_bytes)?;
                    memory.read(&caller, offset + 4, &mut buf_len_bytes)?;

                    let buf_ptr = u32::from_le_bytes(buf_ptr_bytes) as usize;
                    let buf_len = u32::from_le_bytes(buf_len_bytes) as usize;

                    if buf_len == 0 {
                        offset += 8;
                        continue;
                    }

                    let mut buffer = vec![0u8; buf_len];
                    memory.read(&caller, buf_ptr, &mut buffer)?;

                    caller.data_mut().stdout.extend(&buffer);
                    total_written = total_written.saturating_add(buf_len as u32);

                    offset += 8;
                }

                memory.write(
                    &mut caller,
                    nwritten_ptr as usize,
                    &total_written.to_le_bytes(),
                )?;

                Ok(ERRNO_SUCCESS)
            },
        )
        .map_err(|e| e.to_string())?;

    // Minimal WASI fd_read implementation that reads from HostState.stdin.
    linker
        .func_wrap(
            "wasi_snapshot_preview1",
            "fd_read",
            |mut caller: Caller<'_, HostState>,
             fd: i32,
             iovecs_ptr: i32,
             iovecs_len: i32,
             nread_ptr: i32|
             -> Result<i32, WasmError> {
                const ERRNO_SUCCESS: i32 = 0;
                const ERRNO_BADF: i32 = 8;

                // Only stdin (0) is supported.
                if fd != 0 {
                    return Ok(ERRNO_BADF);
                }

                let memory = caller
                    .get_export("memory")
                    .and_then(|ext| ext.into_memory())
                    .ok_or_else(|| WasmError::new("failed to find memory export"))?;

                let mut total_read: u32 = 0;
                let mut offset = iovecs_ptr as usize;

                for _ in 0..iovecs_len {
                    let mut buf_ptr_bytes = [0u8; 4];
                    let mut buf_len_bytes = [0u8; 4];

                    memory.read(&caller, offset, &mut buf_ptr_bytes)?;
                    memory.read(&caller, offset + 4, &mut buf_len_bytes)?;

                    let buf_ptr = u32::from_le_bytes(buf_ptr_bytes) as usize;
                    let buf_len = u32::from_le_bytes(buf_len_bytes) as usize;

                    if buf_len == 0 {
                        offset += 8;
                        continue;
                    }

                    // Take bytes from stdin buffer.
                    let host_state = caller.data_mut();
                    if host_state.stdin.is_empty() {
                        // No more input available: signal to the JS side that we need more.
                        return Err(WasmError::new(AWAIT_STDIN_TRAP_MSG.to_string()));
                    }

                    let bytes_to_read = buf_len.min(host_state.stdin.len());
                    let mut buffer = Vec::with_capacity(bytes_to_read);
                    for _ in 0..bytes_to_read {
                        if let Some(b) = host_state.stdin.pop_front() {
                            buffer.push(b);
                        } else {
                            break;
                        }
                    }
                    drop(host_state);

                    memory.write(&mut caller, buf_ptr, &buffer)?;

                    total_read = total_read.saturating_add(bytes_to_read as u32);
                    offset += 8;
                }

                memory.write(
                    &mut caller,
                    nread_ptr as usize,
                    &total_read.to_le_bytes(),
                )?;

                Ok(ERRNO_SUCCESS)
            },
        )
        .map_err(|e| e.to_string())?;

    Ok(())
}

fn js_error<E: ToString>(error: E) -> JsValue {
    JsValue::from_str(&error.to_string())
}

#[wasm_bindgen]
pub enum StepStatus {
    Completed = 0,
    InProgress = 1,
    AwaitingInput = 2,
}
