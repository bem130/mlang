use mylang_core::analyze_source;
use mylang_wasm_codegen::WasmGenerator;
use wasm_bindgen::prelude::*;
use wasmi::{
    Caller, Config, Engine, Instance, Linker, Module, Store, TypedFunc,
    core::{Trap, TrapCode},
};
const AWAIT_STDIN_TRAP_MSG: &str = "Awaiting Stdin";

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
pub enum StepStatus {
    InProgress,
    Completed,
}

#[wasm_bindgen]
pub struct StepRunner {
    wat: String,
    engine: Engine,
    module: Module,
    store: Store<HostState>,
    instance: Instance,
    start: TypedFunc<(), ()>,
    finished: bool,
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

#[wasm_bindgen]
impl StepRunner {
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str) -> Result<StepRunner, JsValue> {
        let wat = generate_wat(source).map_err(js_error)?;
        StepRunner::from_wat(wat)
    }

    pub fn wat(&self) -> String {
        self.wat.clone()
    }

    pub fn stdout(&self) -> String {
        read_stdout(&self.store)
    }

    pub fn is_complete(&self) -> bool {
        self.finished
    }

    pub fn step(&mut self, fuel: u64) -> Result<StepStatus, JsValue> {
        if self.finished {
            return Ok(StepStatus::Completed);
        }

        self.store
            .add_fuel(fuel)
            .map_err(|error| js_error(error.to_string()))?;

        match self.start.call(&mut self.store, ()) {
            Ok(()) => {
                self.finished = true;
                Ok(StepStatus::Completed)
            }
            Err(trap) => match trap.trap_code() {
                Some(TrapCode::OutOfFuel) => Ok(StepStatus::InProgress),
                _ => Err(js_error(trap)),
            },
        }
    }

    pub fn reset(&mut self) -> Result<(), JsValue> {
        let (store, instance, start) =
            instantiate_module(&self.engine, &self.module).map_err(js_error)?;
        self.store = store;
        self.instance = instance;
        self.start = start;
        self.finished = false;
        Ok(())
    }

    fn from_wat(wat: String) -> Result<StepRunner, JsValue> {
        let mut config = Config::default();
        config.consume_fuel(true);
        let engine = Engine::new(&config);
        let wasm_binary = wat::parse_str(&wat).map_err(js_error)?;
        let module = Module::new(&engine, &*wasm_binary).map_err(js_error)?;
        let (store, instance, start) = instantiate_module(&engine, &module).map_err(js_error)?;

        Ok(StepRunner {
            wat,
            engine,
            module,
            store,
            instance,
            start,
            finished: false,
        })
    }
}

fn generate_wat(source: &str) -> Result<String, String> {
    let analysis_result = analyze_source(source.trim(),false).map_err(|e| e.to_string())?;

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
    let wasm_binary = wat::parse_str(wat_code).map_err(|e| e.to_string())?;
    let module = Module::new(&engine, &*wasm_binary).map_err(|e| e.to_string())?;
    let (mut store, _instance, start) = instantiate_module(&engine, &module)?;

    start.call(&mut store, ()).map_err(|e| e.to_string())?;

    Ok(read_stdout(&store))
}

fn instantiate_module(
    engine: &Engine,
    module: &Module,
) -> Result<(Store<HostState>, Instance, TypedFunc<(), ()>), String> {
    let mut store = Store::new(engine, HostState::default());
    let linker = create_linker(engine)?;
    let instance = linker
        .instantiate(&mut store, module)
        .map_err(|e| e.to_string())?
        .start(&mut store)
        .map_err(|e| e.to_string())?;
    let start = instance
        .get_typed_func::<(), ()>(&store, "_start")
        .map_err(|_| "Wasmモジュールに関数 '_start' が見つかりません".to_string())?;

    Ok((store, instance, start))
}

fn create_linker(engine: &Engine) -> Result<Linker<HostState>, String> {
    let mut linker = <Linker<HostState>>::new(engine);

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

    Ok(linker)
}

fn read_stdout(store: &Store<HostState>) -> String {
    String::from_utf8_lossy(&store.data().stdout).into_owned()
}

fn js_error<E: ToString>(error: E) -> JsValue {
    JsValue::from_str(&error.to_string())
}
