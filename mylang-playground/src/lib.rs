use wasm_bindgen::prelude::*;
use wasmi::{
    Caller, Config, Engine, Error as WasmError, Extern, Linker, Memory, Module, Store,
    TypedFunc, TypedResumableCall, TypedResumableCallHostTrap, TypedResumableCallOutOfFuel, Val,
};
use wasmi::errors::HostError;
use std::collections::VecDeque;
use mylang_core::analyze_source;
use mylang_wasm_codegen::WasmGenerator;

const AWAIT_STDIN_TRAP_MSG: &str = "Awaiting Stdin";

/// ホスト側で保持する状態（stdout バッファと stdin バッファ）
struct HostState {
    stdout: Vec<u8>,
    stdin: VecDeque<u8>,
    pending_read: Option<PendingRead>,
}

struct PendingRead {
    iovecs_ptr: u32,
    iovecs_len: u32,
    nread_ptr: u32,
}

impl Default for HostState {
    fn default() -> Self {
        Self {
            stdout: Vec::new(),
            stdin: VecDeque::new(),
            pending_read: None,
        }
    }
}

/// 標準入力待ちを表すホストエラー
#[derive(Debug)]
struct AwaitStdin;

impl core::fmt::Display for AwaitStdin {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{AWAIT_STDIN_TRAP_MSG}")
    }
}

impl HostError for AwaitStdin {}

/// ステップ実行の状態
enum ExecState {
    /// まだ `_start` を呼んでいない
    NotStarted,
    /// 標準入力待ちでホストトラップしている
    WaitingForInput(TypedResumableCallHostTrap<()>),
    /// Fuel 切れで一時停止している
    OutOfFuel(TypedResumableCallOutOfFuel<()>),
    /// 終了（成功・失敗どちらも含む）
    Finished,
}

fn js_error<E: ToString>(error: E) -> JsValue {
    JsValue::from_str(&error.to_string())
}

/// step の返り値（wasm.d.ts と数値を合わせる）
#[wasm_bindgen]
pub enum StepStatus {
    InProgress = 0,
    Completed = 1,
    AwaitingInput = 2,
}

/// Playground の「WAT」タブ用
#[wasm_bindgen]
pub fn compile_to_wat(source: &str) -> Result<String, JsValue> {
    generate_wat(source).map_err(js_error)
}

fn generate_wat(source: &str) -> Result<String, String> {
    let analysis_result =
        analyze_source(source.trim(), false).map_err(|e| e.to_string())?;

    let mut wasm_generator = WasmGenerator::new(
        analysis_result.function_table,
        analysis_result.string_headers,
        analysis_result.static_offset,
    );

    wasm_generator
        .generate(&analysis_result.typed_ast)
        .map_err(|e| e.to_string())
}

/// Playground 用に必要な最小限の WASI (stdout / stdin) をリンクする
fn link_wasi_functions(linker: &mut Linker<HostState>) -> Result<(), String> {
    // fd_write: stdout/stderr → HostState.stdout
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

                // stdout(1), stderr(2) のみ対応
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

    // fd_read: HostState.stdin → stdin
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

                if fd != 0 {
                    // stdin 以外は未対応
                    return Ok(ERRNO_BADF);
                }

                let memory = caller
                    .get_export("memory")
                    .and_then(|ext| ext.into_memory())
                    .ok_or_else(|| WasmError::new("failed to find memory export"))?;

                // まずは既にバッファされている stdin から読めるだけ読む
                let mut read_bytes: u32 = 0;
                let mut offset = iovecs_ptr as usize;

                for _ in 0..iovecs_len {
                    let mut buf_ptr_bytes = [0u8; 4];
                    let mut buf_len_bytes = [0u8; 4];

                    memory
                        .read(&caller, offset, &mut buf_ptr_bytes)
                        .map_err(|_| WasmError::new("pointer out of bounds"))?;
                    memory
                        .read(&caller, offset + 4, &mut buf_len_bytes)
                        .map_err(|_| WasmError::new("pointer out of bounds"))?;

                    let buf_ptr = u32::from_le_bytes(buf_ptr_bytes) as usize;
                    let buf_len = u32::from_le_bytes(buf_len_bytes) as usize;

                    if buf_len == 0 {
                        offset += 8;
                        continue;
                    }

                    // &mut HostState を長く保持しないよう、一時バッファに移してから書き込む
                    let mut tmp: Vec<u8> = Vec::new();
                    for _ in 0..buf_len {
                        let maybe_byte = {
                            // このブロックの中だけ &mut 借用
                            caller.data_mut().stdin.pop_front()
                        };
                        let Some(b) = maybe_byte else {
                            break;
                        };
                        tmp.push(b);
                    }

                    if tmp.is_empty() {
                        // もうバッファされている入力がない
                        break;
                    }

                    memory
                        .write(&mut caller, buf_ptr, &tmp)
                        .map_err(|_| WasmError::new("buffer out of bounds"))?;
                    read_bytes = read_bytes.saturating_add(tmp.len() as u32);

                    offset += 8;
                }

                if read_bytes > 0 {
                    // バッファ分を読めたのでそのまま成功扱い
                    memory
                        .write(
                            &mut caller,
                            nread_ptr as usize,
                            &read_bytes.to_le_bytes(),
                        )
                        .map_err(|_| WasmError::new("pointer out of bounds"))?;
                    return Ok(ERRNO_SUCCESS);
                }

                // ここまで来ると「今は読むものがない」ので、pending_read を登録して AwaitStdin で一時停止
                {
                    let host = caller.data_mut();
                    host.pending_read = Some(PendingRead {
                        iovecs_ptr: iovecs_ptr as u32,
                        iovecs_len: iovecs_len as u32,
                        nread_ptr: nread_ptr as u32,
                    });
                }

                Err(WasmError::host(AwaitStdin))
            },
        )
        .map_err(|e| e.to_string())?;

    Ok(())
}

/// 実行用の Store / Memory / `_start` を生成するヘルパ
fn instantiate_for_execution(
    engine: &Engine,
    module: &Module,
) -> Result<(Store<HostState>, Memory, TypedFunc<(), ()>), String> {
    let mut store = Store::new(engine, HostState::default());
    let mut linker = Linker::new(engine);

    link_wasi_functions(&mut linker)?;

    let instance = linker
        .instantiate(&mut store, module)
        .map_err(|e| e.to_string())?
        .start(&mut store)
        .map_err(|e| e.to_string())?;

    let memory = instance
        .get_export(&store, "memory")
        .and_then(Extern::into_memory)
        .ok_or_else(|| "failed to find `memory` export".to_string())?;

    let start_func = instance
        .get_export(&store, "_start")
        .and_then(Extern::into_func)
        .ok_or_else(|| "failed to find `_start` export".to_string())?
        .typed::<(), ()>(&store)
        .map_err(|e| e.to_string())?;

    Ok((store, memory, start_func))
}

#[wasm_bindgen]
pub struct StepRunner {
    engine: Engine,
    module: Module,
    store: Store<HostState>,
    memory: Memory,
    start_func: TypedFunc<(), ()>,
    wat: String,
    state: ExecState,
}

#[wasm_bindgen]
impl StepRunner {
    #[wasm_bindgen(constructor)]
    pub fn new(source: &str) -> Result<StepRunner, JsValue> {
        // fuel metering 有効化
        let mut config = Config::default();
        config.consume_fuel(true);
        let engine = Engine::new(&config);

        // ソースを WAT → Wasm バイナリにコンパイル
        let wat = generate_wat(source).map_err(js_error)?;
        let wasm_binary = wat::parse_str(&wat).map_err(js_error)?;
        let module = Module::new(&engine, &*wasm_binary).map_err(js_error)?;

        let (store, memory, start_func) =
            instantiate_for_execution(&engine, &module).map_err(js_error)?;

        Ok(StepRunner {
            engine,
            module,
            store,
            memory,
            start_func,
            wat,
            state: ExecState::NotStarted,
        })
    }

    /// stdout バッファの内容を文字列で返し、バッファをクリア
    pub fn stdout(&mut self) -> String {
        let output = String::from_utf8_lossy(&self.store.data().stdout).to_string();
        self.store.data_mut().stdout.clear();
        output
    }

    /// 実行が終了しているかどうか
    pub fn is_complete(&self) -> bool {
        matches!(self.state, ExecState::Finished)
    }

    /// コンパイルされた WAT を返す
    pub fn wat(&self) -> String {
        self.wat.clone()
    }

    /// fuel 分だけ `_start` の実行を進める
    pub fn step(&mut self, fuel: u64) -> StepStatus {
        match self.step_internal(fuel) {
            Ok(status) => status,
            Err(msg) => {
                // エラーは stdout に出して Completed 扱いにする
                let full = format!("Runtime error: {msg}\n");
                self.store.data_mut().stdout.extend(full.as_bytes());
                self.state = ExecState::Finished;
                StepStatus::Completed
            }
        }
    }

    fn step_internal(&mut self, fuel: u64) -> Result<StepStatus, String> {
        if matches!(self.state, ExecState::Finished) {
            return Ok(StepStatus::Completed);
        }

        // fuel をセット
        self.store
            .set_fuel(fuel)
            .map_err(|e| e.to_string())?;

        // 現在の状態に応じて実行を進める
        let resumable = match core::mem::replace(&mut self.state, ExecState::NotStarted) {
            ExecState::NotStarted => {
                // 初回: `_start` を call_resumable で開始
                self.start_func
                    .call_resumable(&mut self.store, ())
            }
            ExecState::OutOfFuel(out) => {
                // fuel 切れから再開
                out.resume(&mut self.store)
            }
            ExecState::WaitingForInput(host_trap) => {
                // JS 側がまだ入力をくれていない
                if self.store.data().stdin.is_empty() {
                    self.state = ExecState::WaitingForInput(host_trap);
                    return Ok(StepStatus::AwaitingInput);
                }

                // pending_read に基づいて stdin をメモリに書き込む
                self.apply_pending_stdin().map_err(|e| e.to_string())?;

                // fd_read の戻り値として ERRNO_SUCCESS(0) を渡して再開
                let resume_vals = [Val::I32(0)];
                host_trap.resume(&mut self.store, &resume_vals)
            }
            ExecState::Finished => {
                self.state = ExecState::Finished;
                return Ok(StepStatus::Completed);
            }
        };

        let resumable = resumable.map_err(|e| e.to_string())?;

        match resumable {
            TypedResumableCall::Finished(()) => {
                self.state = ExecState::Finished;
                Ok(StepStatus::Completed)
            }
            TypedResumableCall::OutOfFuel(out) => {
                self.state = ExecState::OutOfFuel(out);
                Ok(StepStatus::InProgress)
            }
            TypedResumableCall::HostTrap(host_trap) => {
                let host_err = host_trap.host_error();
                let msg = host_err.to_string();
                if msg.contains(AWAIT_STDIN_TRAP_MSG) {
                    // 標準入力待ち
                    self.state = ExecState::WaitingForInput(host_trap);
                    Ok(StepStatus::AwaitingInput)
                } else {
                    // それ以外のホストエラーは普通のエラーとして終了
                    self.state = ExecState::Finished;
                    Err(msg)
                }
            }
        }
    }

    /// JS から 1 行分の入力を渡す（末尾に改行を付加）
    pub fn provide_stdin(&mut self, input: &str) {
        let data = self.store.data_mut();
        data.stdin.extend(input.as_bytes());
        data.stdin.push_back(b'\n');
    }

    /// 同じモジュールでもう一度最初から実行したいとき用
    pub fn reset(&mut self) {
        match instantiate_for_execution(&self.engine, &self.module) {
            Ok((store, memory, start_func)) => {
                self.store = store;
                self.memory = memory;
                self.start_func = start_func;
                self.state = ExecState::NotStarted;
            }
            Err(e) => {
                let msg = format!("Failed to reset: {e}\n");
                self.store.data_mut().stdout.extend(msg.as_bytes());
                self.state = ExecState::Finished;
            }
        }
    }
}

impl StepRunner {
    /// pending_read の情報に従って HostState.stdin からメモリにデータを書き込む
    fn apply_pending_stdin(&mut self) -> Result<(), &'static str> {
        let pending = {
            let host = self.store.data_mut();
            host.pending_read
                .take()
                .ok_or("no pending stdin read")?
        };

        let memory = self.memory;
        let mut read_bytes: u32 = 0;
        let mut offset = pending.iovecs_ptr as usize;

        for _ in 0..pending.iovecs_len {
            let mut buf_ptr_bytes = [0u8; 4];
            let mut buf_len_bytes = [0u8; 4];

            memory
                .read(&self.store, offset, &mut buf_ptr_bytes)
                .map_err(|_| "pointer out of bounds")?;
            memory
                .read(&self.store, offset + 4, &mut buf_len_bytes)
                .map_err(|_| "pointer out of bounds")?;

            let buf_ptr = u32::from_le_bytes(buf_ptr_bytes) as usize;
            let buf_len = u32::from_le_bytes(buf_len_bytes) as usize;

            if buf_len == 0 {
                offset += 8;
                continue;
            }

            // stdin キューから必要なだけ取り出す
            let mut chunk: Vec<u8> = Vec::new();
            for _ in 0..buf_len {
                let maybe_byte = {
                    let host = self.store.data_mut();
                    host.stdin.pop_front()
                };
                let Some(b) = maybe_byte else {
                    break;
                };
                chunk.push(b);
            }

            if chunk.is_empty() {
                break;
            }

            memory
                .write(&mut self.store, buf_ptr, &chunk)
                .map_err(|_| "buffer out of bounds")?;
            read_bytes = read_bytes.saturating_add(chunk.len() as u32);

            offset += 8;
        }

        // 読んだバイト数を nread_ptr に書き込む
        memory
            .write(
                &mut self.store,
                pending.nread_ptr as usize,
                &read_bytes.to_le_bytes(),
            )
            .map_err(|_| "pointer out of bounds")?;

        Ok(())
    }
}
