import type { LoreUsageStats } from "./usage-types.ts";

export type JsonValue =
  | null
  | boolean
  | number
  | string
  | JsonValue[]
  | { [key: string]: JsonValue };

export type JsonObject = { [key: string]: JsonValue };

export type McpContent = {
  type: string;
  text?: string;
  [key: string]: unknown;
};

export type McpTool = {
  name: string;
  description?: string;
  inputSchema: JsonObject;
};

export type LoreStructuredToolResult = {
  content: McpContent[];
  isError?: boolean;
  structuredContent: unknown;
};

export type LoreConfig = {
  enabled: boolean;
  command?: string;
  args: string[];
  env: Record<string, string>;
  cwd?: string;
  startupTimeoutMs: number;
  defaultToolTimeoutMs: number;
  toolTimeoutMs: Record<string, number>;
  summaryTimeoutMs: number;
  maxInlineDiffBytes: number;
  allowToolOverride: boolean;
  stateDir: string;
  tools: {
    disabled: string[];
  };
  recovery: {
    compilation: boolean;
    tests: boolean;
  };
};

export type LoreProcessConfig = LoreConfig & {
  command: string;
};

export type LoreCallOptions = {
  timeoutMs?: number;
  signal?: AbortSignal;
};

export type KnowledgeSnapshot = {
  hashes: string[];
};

export type RecoveryObligations = {
  compilationPending: boolean;
  testsPending: boolean;
};

export type ValidationToolName = "reloadHomeModules" | "runTestSuite";

export type RecoveryFinalizationFailure = {
  stage: "missing-context-marker" | "missing-final-tool-result" | "context-range" | "diff" | "summary" | "commit";
  message: string;
  failedAt: number;
};

export type RecoveryState =
  | { phase: "inactive" }
  | {
      phase: "active";
      recoveryId: string;
      contextMarker: string;
      startValidationToolName: ValidationToolName;
      startValidationToolCallId: string;
      startEntryId?: string;
      startedAt: number;
      reason: string;
      baselineId: string;
      compilationPending: boolean;
      testsPending: boolean;
    }
  | {
      phase: "readyToFinalize";
      recoveryId: string;
      contextMarker: string;
      startValidationToolName: ValidationToolName;
      startValidationToolCallId: string;
      finalValidationToolCallId: string;
      startEntryId?: string;
      startedAt: number;
      reason: string;
      baselineId: string;
      compilationPending: false;
      testsPending: false;
    }
  | {
      phase: "finalizationFailed";
      recoveryId: string;
      contextMarker: string;
      startValidationToolName: ValidationToolName;
      startValidationToolCallId: string;
      finalValidationToolCallId: string;
      startEntryId?: string;
      startedAt: number;
      reason: string;
      baselineId: string;
      compilationPending: false;
      testsPending: false;
      failure: RecoveryFinalizationFailure;
    };

export type RecoveryDiff = {
  reliable: boolean;
  reason?: string;
  changedPaths: string[];
  stats: {
    filesChanged: number;
    additions: number;
    deletions: number;
  };
  inlinePatch?: string;
  patchPath?: string;
  truncated: boolean;
};

export type CompletedRecovery = {
  recoveryId: string;
  startEntryId?: string;
  startValidationToolCallId: string;
  finalValidationToolCallId: string;
  summary: string;
  contextReplacement: string;
  diff: RecoveryDiff;
  tokenMetrics?: RecoveryTokenMetrics;
  completedAt: number;
};

export type RecoveryTokenMetrics = {
  originalRecoveryTokens: number;
  summaryReplacementTokens: number;
  estimated: true;
};

export type LoreSessionEvent =
  | { kind: "knowledgeSnapshot"; snapshot: KnowledgeSnapshot }
  | { kind: "knowledgeReset"; snapshot: KnowledgeSnapshot }
  | { kind: "recoveryState"; state: RecoveryState }
  | { kind: "recoveryAbandoned"; state: { phase: "inactive" }; recoveryId: string; abandonedAt: number }
  | { kind: "completedRecovery"; completed: CompletedRecovery }
  | { kind: "uiMarker"; marker: "recovery-start" | "recovery-complete"; recoveryId: string };

export type PiEntry = {
  id?: string;
  role?: string;
  type?: string;
  content?: unknown;
  text?: string;
  details?: unknown;
  createdAt?: number;
  [key: string]: unknown;
};

export type PiToolRegistration = {
  name: string;
  label?: string;
  description?: string;
  promptSnippet?: string;
  promptGuidelines?: string[];
  executionMode?: "sequential" | "parallel";
  renderShell?: "self";
  renderCall?: (args: unknown, theme: unknown, context: PiToolRenderContext) => PiComponent;
  renderResult?: (
    result: PiToolResult,
    options: PiToolRenderOptions,
    theme: unknown,
    context: PiToolRenderContext,
  ) => PiComponent;
  parameters: JsonObject;
  execute: (
    toolCallId: string,
    params: unknown,
    signal: AbortSignal | undefined,
    onUpdate: unknown,
    context: unknown,
  ) => Promise<unknown>;
};

export type PiToolResult = {
  content?: McpContent[];
  details?: unknown;
  isError?: boolean;
  [key: string]: unknown;
};

export type PiToolRenderOptions = {
  expanded: boolean;
  isPartial: boolean;
  [key: string]: unknown;
};

export type PiToolRenderContext = {
  args?: unknown;
  state?: Record<string, unknown>;
  lastComponent?: unknown;
  invalidate?: () => void;
  [key: string]: unknown;
};

export type PiComponent = {
  render: (width: number) => string[];
  invalidate: () => void;
  handleInput?: (data: string) => void;
  wantsKeyRelease?: boolean;
};

export type PiCustomMessage = {
  customType: string;
  content: unknown;
  display: boolean;
  details?: unknown;
};

export type PiSendMessageOptions = {
  triggerTurn?: boolean;
  deliverAs?: "steer" | "followUp" | "nextTurn";
};

export type PiHost = {
  projectDir?: string;
  cwd?: string;
  getConfig?: (name?: string) => unknown;
  registerTool?: (tool: PiToolRegistration) => void | Promise<void>;
  hasTool?: (name: string) => boolean;
  onLoreToolsRegistered?: (toolNames: string[]) => void | Promise<void>;
  on?: (event: string, handler: (...args: unknown[]) => unknown) => void;
  appendEntry?: (entry: PiEntry) => Promise<PiEntry | void> | PiEntry | void;
  sendMessage?: (
    message: PiCustomMessage,
    options?: PiSendMessageOptions,
  ) => Promise<void> | void;
  appendDisplayMessageImmediately?: (
    message: PiCustomMessage,
  ) => Promise<"appended" | "already-present"> | "appended" | "already-present";
  getActiveBranchEntries?: () => PiEntry[] | Promise<PiEntry[]> | undefined;
  getCurrentEntryId?: () => string | undefined;
  setStatus?: (key: string, text: string, options?: unknown) => void | Promise<void>;
  clearStatus?: (key: string) => void | Promise<void>;
  notify?: (message: string, options?: unknown) => void | Promise<void>;
  generateText?: (request: {
    prompt: string;
    timeoutMs: number;
    tools?: false;
    signal?: AbortSignal;
  }) => Promise<string>;
  generateTextFromMessages?: (request: {
    messages: unknown[];
    prompt: string;
    timeoutMs: number;
    tools?: false;
    signal?: AbortSignal;
  }) => Promise<string>;
  projectTrusted?: boolean;
  [key: string]: unknown;
};

export type ExtensionRuntime = {
  start: () => Promise<void>;
  startResolved: (processConfig: LoreProcessConfig) => Promise<{ ok: true; registeredToolNames: string[] } | { ok: false; error: Error }>;
  stop: () => Promise<void>;
  restartLore: () => Promise<void>;
  listAvailableToolNames: () => Promise<string[]>;
  abandonRecovery: () => Promise<void>;
  processContext: (input: { rawMessages: unknown[]; normalizedEntries: PiEntry[] }) => Promise<PiEntry[]>;
  getUsageStats: () => Promise<LoreUsageStats>;
  getState: () => {
    knowledge: KnowledgeSnapshot;
    recovery: RecoveryState;
    completedRecoveries: CompletedRecovery[];
    registeredToolNames: string[];
    startupError?: string;
  };
};
