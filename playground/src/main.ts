import "./style.css";
import init, {
  compile_to_wat,
  StepRunner,
  StepStatus,
} from "../pkg/mlang_playground.js";
import { Terminal } from 'xterm';
import { FitAddon } from 'xterm-addon-fit';
import 'xterm/css/xterm.css';

type WasmInit = ReturnType<typeof init>;
const wasmReady: WasmInit = init();
const STEP_FUEL = 1_000_000n;
const STEP_INTERVAL_MS = 16;

const sourceInput = document.getElementById("source") as HTMLTextAreaElement;
const watOutput = document.getElementById("wat") as HTMLTextAreaElement;
const compileButton = document.getElementById("compile") as HTMLButtonElement;
const runButton = document.getElementById("run") as HTMLButtonElement;
const stepStartButton = document.getElementById("step-start") as HTMLButtonElement;
const stepPauseButton = document.getElementById("step-pause") as HTMLButtonElement;
const stepStopButton = document.getElementById("step-stop") as HTMLButtonElement;
const statusElement = document.getElementById("status") as HTMLDivElement;
const docsLink = document.getElementById("docs-link") as HTMLAnchorElement | null;
const terminalContainer = document.getElementById("terminal") as HTMLDivElement;

let stepRunner: StepRunner | null = null;
let stepIntervalId: number | null = null;
let currentInputBuffer = '';

const term = new Terminal({
  cursorBlink: true,
  convertEol: true,
  theme: {
    background: '#1e1e1e',
    foreground: '#d4d4d4',
  },
});
const fitAddon = new FitAddon();
term.loadAddon(fitAddon);
term.open(terminalContainer);
fitAddon.fit();

window.addEventListener('resize', () => fitAddon.fit());

function writeToTerminal(data: string) {
  term.write(data.replace(/\n/g, '\r\n'));
}

term.onData(e => {
  if (!stepRunner || stepRunner.is_complete()) {
    return;
  }
  
  switch (e) {
    case '\r': // Enter
      writeToTerminal('\r\n');
      stepRunner.provide_stdin(currentInputBuffer + '\n');
      currentInputBuffer = '';
      resumeStepping();
      break;
    case '\u007F': // Backspace
      if (currentInputBuffer.length > 0) {
        term.write('\b \b');
        currentInputBuffer = currentInputBuffer.slice(0, -1);
      }
      break;
    default:
      if (e >= String.fromCharCode(0x20) && e <= String.fromCharCode(0x7e)) {
        currentInputBuffer += e;
        term.write(e);
      }
  }
});

const DEFAULT_SOURCE = `// 正しい構文に修正したコード
fn greet |name: string|->() {
    println(string_concat("Hello, ", name));
}

fn main ||->() {
    greet("Web");

    println("\\nPlease enter your name:");
    let name read_line();
    println(string_concat("Hello from terminal, ", name));
}
`;

if (sourceInput.value.trim().length === 0) {
  sourceInput.value = DEFAULT_SOURCE;
}

if (docsLink) {
  const configuredDocsUrl = import.meta.env.VITE_DOCUMENTATION_URL;
  const fallbackUrl = docsLink.dataset.docsUrl
    ? new URL(docsLink.dataset.docsUrl, window.location.href).toString()
    : new URL("../doc/", window.location.href).toString();
  docsLink.href = configuredDocsUrl?.trim() ? configuredDocsUrl : fallbackUrl;
  docsLink.rel = "noopener noreferrer";
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

function updateStepControlState(isRunning: boolean, hasRunner: boolean): void {
  runButton.disabled = isRunning;
  stepStartButton.disabled = isRunning;
  stepPauseButton.disabled = !hasRunner || !isRunning;
  stepStopButton.disabled = !hasRunner;
}

function clearStepTimer(): void {
  if (stepIntervalId !== null) {
    window.clearInterval(stepIntervalId);
    stepIntervalId = null;
  }
}

function disposeStepping(message?: string, isError = false): void {
  clearStepTimer();
  if (stepRunner) {
    stepRunner.free();
    stepRunner = null;
  }
  updateStepControlState(false, false);
  if (message) {
    setStatus(message, isError);
  }
}

function pumpStep(): void {
  if (!stepRunner) {
    return;
  }

  try {
    const status = stepRunner.step(STEP_FUEL);
    const stdout = stepRunner.stdout();
    if (stdout) {
      writeToTerminal(stdout);
    }

    if (status === StepStatus.Completed) {
      disposeStepping("実行が完了しました。");
    } else if (status === StepStatus.AwaitingInput) {
      clearStepTimer();
      updateStepControlState(false, true);
      setStatus("ターミナルからの入力を待っています...");
      term.focus();
    }
  } catch (error) {
    disposeStepping(`実行時エラー: ${formatError(error)}`, true);
  }
}

async function startNewRun() {
  disposeStepping();
  term.reset();
  
  try {
    await wasmReady;

    // ここを変更: StepRunner.wat() の代わりに compile_to_wat を使う
    const wat = compile_to_wat(sourceInput.value);
    watOutput.value = wat;

    stepRunner = new StepRunner(sourceInput.value);
    resumeStepping();
  } catch(error) {
    disposeStepping(`実行開始に失敗しました: ${formatError(error)}`, true);
  }
}

function resumeStepping() {
  if (!stepRunner) return;
  
  updateStepControlState(true, true);
  setStatus("実行中...");
  
  if (stepIntervalId === null) {
    stepIntervalId = window.setInterval(pumpStep, STEP_INTERVAL_MS);
  }
  pumpStep();
}

compileButton.addEventListener("click", async () => {
  compileButton.disabled = true;
  setStatus("コンパイル中...");
  disposeStepping();
  term.reset();

  try {
    await wasmReady;
    const wat = compile_to_wat(sourceInput.value);
    watOutput.value = wat;
    setStatus("コンパイルが完了しました。");
  } catch (error) {
    setStatus(`コンパイルに失敗しました: ${formatError(error)}`, true);
  } finally {
    compileButton.disabled = false;
  }
});

runButton.addEventListener("click", startNewRun);

stepStartButton.addEventListener("click", () => {
  if (!stepRunner || stepRunner.is_complete()) {
    startNewRun();
  } else {
    resumeStepping();
  }
});

stepPauseButton.addEventListener("click", () => {
  if (!stepRunner || stepIntervalId === null) return;
  clearStepTimer();
  updateStepControlState(false, true);
  setStatus("一時停止しました。");
});

stepStopButton.addEventListener("click", () => {
  if (!stepRunner) {
    setStatus("実行されていません。");
    return;
  }
  disposeStepping("実行を停止しました。");
  term.reset();
});

wasmReady
  .then(() => {
    setStatus("Wasmランタイムの初期化が完了しました。", false);
  })
  .catch((error) => {
    setStatus(`初期化に失敗しました: ${formatError(error)}`, true);
    [compileButton, runButton, stepStartButton, stepPauseButton, stepStopButton].forEach(
      (btn) => (btn.disabled = true)
    );
  });

