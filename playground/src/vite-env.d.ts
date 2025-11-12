/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_DOCUMENTATION_URL?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
