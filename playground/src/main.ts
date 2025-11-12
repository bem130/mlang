import "./style.css";
import init, {
  compile_to_wat,
  run as runProgram,
  StepRunner,
  StepStatus,
} from "../pkg/mlang_playground.js";

type WasmInit = ReturnType<typeof init>;
const wasmReady: WasmInit = init();
const STEP_FUEL = 1000n;
const STEP_FUEL_DISPLAY = Number(STEP_FUEL);
const STEP_INTERVAL_MS = 30;

const sourceInput = document.getElementById("source") as HTMLTextAreaElement;
const watOutput = document.getElementById("wat") as HTMLTextAreaElement;
const runOutput = document.getElementById("output") as HTMLTextAreaElement;
const compileButton = document.getElementById("compile") as HTMLButtonElement;
const runButton = document.getElementById("run") as HTMLButtonElement;
const stepStartButton = document.getElementById("step-start") as HTMLButtonElement;
const stepPauseButton = document.getElementById("step-pause") as HTMLButtonElement;
const stepStopButton = document.getElementById("step-stop") as HTMLButtonElement;
const statusElement = document.getElementById("status") as HTMLDivElement;

let stepRunner: StepRunner | null = null;
let stepIntervalId: number | null = null;

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

function updateStepControlState(isRunning: boolean, hasRunner: boolean): void {
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

function disposeStepping(): void {
  clearStepTimer();

  if (stepRunner) {
    stepRunner.free();
    stepRunner = null;
  }

  updateStepControlState(false, false);
}

function resetSteppingState(message?: string, isError = false): void {
  disposeStepping();

  if (message) {
    setStatus(message, isError);
  }
}

function handleStepError(error: unknown): void {
  resetSteppingState(`ステップ実行に失敗しました: ${formatError(error)}`, true);
}

function pumpStep(): void {
  if (!stepRunner) {
    return;
  }

  try {
    const status = stepRunner.step(STEP_FUEL);
    runOutput.value = stepRunner.stdout();

    if (status === StepStatus.Completed) {
      clearStepTimer();
      updateStepControlState(false, true);
      setStatus("ステップ実行が完了しました。");
    }
  } catch (error) {
    handleStepError(error);
  }
}

updateStepControlState(false, false);

compileButton.addEventListener("click", async () => {
  toggleBusy(compileButton, true);
  setStatus("コンパイラを初期化しています...");
  disposeStepping();

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
  disposeStepping();
  toggleBusy(runButton, true);
  setStatus("Wasmを実行しています...");

  try {
    await wasmReady;
    const result = runProgram(sourceInput.value);
    const wat = result.wat;
    const stdout = result.stdout;
    if (typeof result.free === "function") {
      result.free();
    }
    watOutput.value = wat;
    runOutput.value = stdout;
    setStatus("実行が完了しました。");
  } catch (error) {
    setStatus(`実行に失敗しました: ${formatError(error)}`, true);
  } finally {
    toggleBusy(runButton, false);
  }
});

stepStartButton.addEventListener("click", async () => {
  try {
    await wasmReady;

    if (stepRunner === null) {
      stepRunner = new StepRunner(sourceInput.value);
      watOutput.value = stepRunner.wat();
      runOutput.value = "";
    } else if (stepRunner.is_complete()) {
      stepRunner.reset();
      runOutput.value = "";
    }

    updateStepControlState(true, true);

    if (stepIntervalId === null) {
      stepIntervalId = window.setInterval(pumpStep, STEP_INTERVAL_MS);
    }

    setStatus(`ステップ実行中です（${STEP_FUEL_DISPLAY}命令ずつ実行）`);
    pumpStep();
  } catch (error) {
    handleStepError(error);
  }
});

stepPauseButton.addEventListener("click", () => {
  if (!stepRunner || stepIntervalId === null) {
    return;
  }

  clearStepTimer();
  updateStepControlState(false, true);
  setStatus("ステップ実行を一時停止しました。");
});

stepStopButton.addEventListener("click", () => {
  if (!stepRunner) {
    setStatus("ステップ実行は開始されていません。");
    return;
  }

  resetSteppingState("ステップ実行を停止しました。");
});

wasmReady
  .then(() => {
    setStatus("Wasmランタイムの初期化が完了しました。", false);
  })
  .catch((error) => {
    setStatus(`初期化に失敗しました: ${formatError(error)}`, true);
    compileButton.disabled = true;
    runButton.disabled = true;
    stepStartButton.disabled = true;
    stepPauseButton.disabled = true;
    stepStopButton.disabled = true;
  });
