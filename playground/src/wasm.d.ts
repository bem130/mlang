declare module "../pkg/mlang_playground.js" {
  export default function init(input?: RequestInfo | URL | Response | BufferSource | WebAssembly.Module): Promise<void>;
  export function compile_to_wat(source: string): string;
  export function run(source: string): RunResult;
  export enum StepStatus {
    InProgress,
    Completed,
  }
  export class StepRunner {
    constructor(source: string);
    wat(): string;
    stdout(): string;
    is_complete(): boolean;
    step(fuel: bigint): StepStatus;
    reset(): void;
    free(): void;
  }

  export class RunResult {
    readonly wat: string;
    readonly stdout: string;
    free(): void;
  }
}
