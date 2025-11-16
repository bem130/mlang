declare module "../pkg/mlang_playground.js" {
  export default function init(input?: RequestInfo | URL | Response | BufferSource | WebAssembly.Module): Promise<void>;
  export function compile_to_wat(source: string): string;

  export enum StepStatus {
    InProgress,
    Completed,
    AwaitingInput,
  }
  
  export class StepRunner {
    constructor(source: string);
    wat(): string;
    stdout(): string;
    is_complete(): boolean;
    step(fuel: bigint): StepStatus;
    reset(): void;
    provide_stdin(input: string): void;
    free(): void;
  }
}