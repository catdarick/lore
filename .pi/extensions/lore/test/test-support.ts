import { join } from "node:path";
import type { PiEntry, PiHost, PiToolRegistration } from "../src/types.ts";

export class FakePiHost implements PiHost {
  projectDir: string;
  tools = new Map<string, PiToolRegistration>();
  entries: PiEntry[] = [];
  statuses = new Map<string, string>();
  notices: string[] = [];
  sentMessages: Array<{ message: { customType: string; content: unknown; display: boolean; details?: unknown }; options?: unknown }> = [];
  currentEntryIdValue?: string;
  currentAssistantStartId?: string;
  handlers = new Map<string, ((...args: unknown[]) => unknown)[]>();
  summary = "Generated recovery summary.";
  summaryFromMessages = "Generated recovery summary from messages.";
  generatedTextFromMessagesRequests: Array<{ messages: unknown[]; prompt: string; timeoutMs: number }> = [];

  constructor(projectDir: string) {
    this.projectDir = projectDir;
  }

  getConfig(): unknown {
    return {
      command: "python3",
      args: [join(this.projectDir, ".pi/extensions/lore/test/fake-lore-mcp.py")],
      cwd: this.projectDir,
      startupTimeoutMs: 5_000,
      defaultToolTimeoutMs: 5_000,
      toolTimeoutMs: { reloadHomeModules: 100, runTestSuite: 5_000, echo: 5_000 },
      summaryTimeoutMs: 5_000,
      maxInlineDiffBytes: 50_000,
      stateDir: ".pi/extensions/lore/state-test",
    };
  }

  registerTool(tool: PiToolRegistration): void {
    this.tools.set(tool.name, tool);
  }

  hasTool(name: string): boolean {
    return this.tools.has(name);
  }

  on(event: string, handler: (...args: unknown[]) => unknown): void {
    const handlers = this.handlers.get(event) ?? [];
    handlers.push(handler);
    this.handlers.set(event, handlers);
  }

  async emit(event: string, ...args: unknown[]): Promise<void> {
    for (const handler of this.handlers.get(event) ?? []) {
      await handler(...args);
    }
  }

  appendEntry(entry: PiEntry): PiEntry {
    const withId = { ...entry, id: entry.id ?? `entry-${this.entries.length + 1}` };
    this.entries.push(withId);
    return withId;
  }

  getActiveBranchEntries(): PiEntry[] {
    return this.entries;
  }

  getCurrentEntryId(): string | undefined {
    return this.currentEntryIdValue;
  }

  getCurrentAssistantSequenceStartEntryId(): string | undefined {
    return this.currentAssistantStartId;
  }

  setStatus(key: string, text: string): void {
    this.statuses.set(key, text);
  }

  clearStatus(key: string): void {
    this.statuses.delete(key);
  }

  notify(message: string): void {
    this.notices.push(message);
  }

  sendMessage(
    message: { customType: string; content: unknown; display: boolean; details?: unknown },
    options?: unknown,
  ): void {
    this.sentMessages.push({ message, options });
    this.entries.push({
      id: `entry-${this.entries.length + 1}`,
      type: "custom_message",
      role: "custom",
      customType: message.customType,
      content: message.content,
      details: message.details,
      display: message.display,
    });
  }

  appendDisplayMessageImmediately(message: {
    customType: string;
    content: unknown;
    display: boolean;
    details?: unknown;
  }): "appended" | "already-present" {
    const recoveryId =
      message.details && typeof message.details === "object"
        ? (message.details as { recoveryId?: unknown }).recoveryId
        : undefined;
    if (
      typeof recoveryId === "string" &&
      this.entries.some((entry) => {
        const details = entry.details && typeof entry.details === "object"
          ? (entry.details as { recoveryId?: unknown })
          : undefined;
        return entry.customType === message.customType && details?.recoveryId === recoveryId;
      })
    ) {
      return "already-present";
    }
    this.entries.push({
      id: `entry-${this.entries.length + 1}`,
      type: "custom_message",
      role: "custom",
      customType: message.customType,
      content: message.content,
      details: message.details,
      display: message.display,
    });
    return "appended";
  }

  async generateText(): Promise<string> {
    return this.summary;
  }

  async generateTextFromMessages(request: { messages: unknown[]; prompt: string; timeoutMs: number }): Promise<string> {
    this.generatedTextFromMessagesRequests.push(request);
    return this.summaryFromMessages;
  }
}
