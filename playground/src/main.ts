import "./style.css";
import init, { compile_to_wat, run as runProgram } from "../pkg/mlang_playground.js";

type WasmInit = ReturnType<typeof init>;
const wasmReady: WasmInit = init();

const sourceInput = document.getElementById("source") as HTMLTextAreaElement;
const watOutput = document.getElementById("wat") as HTMLTextAreaElement;
const runOutput = document.getElementById("output") as HTMLTextAreaElement;
const compileButton = document.getElementById("compile") as HTMLButtonElement;
const runButton = document.getElementById("run") as HTMLButtonElement;
const statusElement = document.getElementById("status") as HTMLDivElement;

const DEFAULT_SOURCE = `fn greet(name: string) {
    println string_concat "Hello, " name;
}

fn main() {
    greet "Web";
}
`;

if (sourceInput.value.trim().length === 0) {
  sourceInput.value = DEFAULT_SOURCE;
}

function setStatus(message: string, isError = false): void {
  statusElement.textContent = message;
  statusElement.classList.toggle("status--error", isError);
}

function formatError(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }

  if (typeof error === "string") {
    return error;
  }

  try {
    return JSON.stringify(error);
  } catch (_) {
    return String(error);
  }
}

function toggleBusy(button: HTMLButtonElement, isBusy: boolean): void {
  button.disabled = isBusy;
}

compileButton.addEventListener("click", async () => {
  toggleBusy(compileButton, true);
  setStatus("コンパイラを初期化しています...");

  try {
    await wasmReady;
    const wat = compile_to_wat(sourceInput.value);
    watOutput.value = wat;
    setStatus("WATの生成が完了しました。");
  } catch (error) {
    setStatus(`コンパイルに失敗しました: ${formatError(error)}`, true);
  } finally {
    toggleBusy(compileButton, false);
  }
});

runButton.addEventListener("click", async () => {
  toggleBusy(runButton, true);
  setStatus("Wasmを実行しています...");

  try {
    await wasmReady;
    const result = runProgram(sourceInput.value);
    watOutput.value = result.wat();
    runOutput.value = result.stdout();
    setStatus("実行が完了しました。");
  } catch (error) {
    setStatus(`実行に失敗しました: ${formatError(error)}`, true);
  } finally {
    toggleBusy(runButton, false);
  }
});

wasmReady
  .then(() => {
    setStatus("Wasmランタイムの初期化が完了しました。", false);
  })
  .catch((error) => {
    setStatus(`初期化に失敗しました: ${formatError(error)}`, true);
    compileButton.disabled = true;
    runButton.disabled = true;
  });
