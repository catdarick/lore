import { LoreInfrastructureError } from "./errors.ts";
import { decodeValidationOutcome, isDefinitionTool, renderedText } from "./lore-protocol.ts";
import { adaptMcpSchemaForPiNullableRequiredBug } from "./pi-schema-compat.ts";
import type { KnowledgeSynchronizer } from "./knowledge.ts";
import type { LoreClient } from "./mcp-client.ts";
import type { RecoveryManager } from "./recovery.ts";
import { renderLoreToolCall, renderLoreToolResult } from "./tool-render.ts";
import type { LoreConfig, LoreStructuredToolResult, McpTool, PiHost, PiToolRegistration } from "./types.ts";
import type { LoreRecoveryUi } from "./ui.ts";
import { stableStringify } from "./util.ts";

export class LoreToolProxy {
  private readonly host: PiHost;
  private config: LoreConfig;
  private client: LoreClient;
  private knowledge: KnowledgeSynchronizer;
  private readonly recovery: RecoveryManager;
  private readonly ui: LoreRecoveryUi;

  constructor(input: {
    host: PiHost;
    config: LoreConfig;
    client: LoreClient;
    knowledge: KnowledgeSynchronizer;
    recovery: RecoveryManager;
    ui: LoreRecoveryUi;
  }) {
    this.host = input.host;
    this.config = input.config;
    this.client = input.client;
    this.knowledge = input.knowledge;
    this.recovery = input.recovery;
    this.ui = input.ui;
  }

  replaceRuntime(input: { config: LoreConfig; client: LoreClient; knowledge: KnowledgeSynchronizer }): void {
    this.config = input.config;
    this.client = input.client;
    this.knowledge = input.knowledge;
  }

  async registerAll(): Promise<McpTool[]> {
    const tools = await this.client.listTools();
    const publicTools = tools.filter((tool) => !isPrivateLoreMethod(tool.name) && toolEnabled(this.config, tool.name));
    const names = new Set<string>();
    const registrations: PiToolRegistration[] = [];
    for (const tool of publicTools) {
      if (names.has(tool.name)) {
        throw new Error(`Cannot register Lore tools: duplicate tool name ${tool.name}`);
      }
      names.add(tool.name);
      if (!this.config.allowToolOverride && this.host.hasTool?.(tool.name)) {
        throw new Error(`Cannot register Lore tool ${tool.name}: a Pi tool with that name already exists`);
      }
      registrations.push(this.registrationFor(tool));
    }
    for (const registration of registrations) {
      if (!this.host.registerTool) {
        throw new Error("Cannot register Lore tools: host.registerTool is unavailable");
      }
      await this.host.registerTool(registration);
    }
    return publicTools;
  }

  registrationFor(tool: McpTool): PiToolRegistration {
    return {
      name: tool.name,
      label: tool.name,
      description: tool.description,
      promptSnippet: `Lore MCP tool: ${tool.description ?? tool.name}`,
      promptGuidelines: [
        "For Lore validation tools, treat structured semantic failures as normal tool results to fix, then rerun the relevant validation.",
      ],
      parameters: adaptMcpSchemaForPiNullableRequiredBug(tool.inputSchema),
      executionMode: "sequential",
      renderCall: (args: unknown, theme: unknown, context) => renderLoreToolCall(tool.name, args, theme, context),
      renderResult: (result, options, theme: unknown, context) =>
        renderLoreToolResult(tool.name, result, options, theme, context),
      execute: async (toolCallId: string, params: unknown, signal: AbortSignal | undefined, _onUpdate: unknown, context: unknown) => {
        return this.executeTool(toolCallId, tool.name, params, signal, context);
      },
    };
  }

  private async executeTool(
    toolCallId: string,
    name: string,
    args: unknown,
    signal: AbortSignal | undefined,
    _context: unknown,
  ): Promise<unknown> {
    try {
      await this.knowledge.restoreActiveBranch();
      const result = await this.client.callStructured(name, args, {
        timeoutMs: this.config.toolTimeoutMs[name] ?? this.config.defaultToolTimeoutMs,
        signal,
      });
      const content = ensureVisibleToolContent(name, result);
      const details: Record<string, unknown> = {
        lore: {
          structuredContent: result.structuredContent,
        },
      };
      if (isDefinitionTool(name)) {
        const snapshot = await this.knowledge.captureIfChanged();
        if (snapshot) {
          details.lore = {
            ...(details.lore as Record<string, unknown>),
            knowledgeSnapshot: snapshot,
          };
        }
      }
      const outcome = decodeValidationOutcome(name, result);
      await this.recovery.handleValidation(outcome, renderedText(result), toolCallId);
      return {
        content,
        isError: result.isError,
        details,
      };
    } catch (error) {
      if (error instanceof LoreInfrastructureError) {
        await this.ui.showInfrastructureError(`Lore infrastructure error: ${error.message}`);
      }
      throw error;
    }
  }
}

function toolEnabled(config: LoreConfig, name: string): boolean {
  return !config.tools.disabled.includes(name);
}

function ensureVisibleToolContent(toolName: string, result: LoreStructuredToolResult): LoreStructuredToolResult["content"] {
  if (hasTextContent(result.content)) {
    return result.content;
  }
  const fallback = stableStringify(result.structuredContent);
  return [
    {
      type: "text",
      text: `${toolName} returned no text content. Structured result:\n${truncate(fallback, 8_000)}`,
    },
  ];
}

function hasTextContent(content: LoreStructuredToolResult["content"]): boolean {
  return content.some((part) => typeof part.text === "string" && part.text.trim().length > 0);
}

function truncate(text: string, maxChars: number): string {
  if (text.length <= maxChars) {
    return text;
  }
  const half = Math.floor(maxChars / 2);
  return `${text.slice(0, half)}\n...truncated...\n${text.slice(-half)}`;
}

export function isPrivateLoreMethod(name: string): boolean {
  return name.startsWith("lore/");
}
