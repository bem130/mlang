declare module "../pkg/mlang_playground.js" {
  export default function init(input?: RequestInfo | URL | Response | BufferSource | WebAssembly.Module): Promise<void>;
  export function compile_to_wat(source: string): string;
  export function run(source: string): RunResult;

  export class RunResult {
    wat(): string;
    stdout(): string;
  }
}
