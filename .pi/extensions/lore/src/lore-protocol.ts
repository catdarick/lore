import { LoreProtocolError } from "./errors.ts";
import type { LoreStructuredToolResult, McpContent } from "./types.ts";

export type ValidationToolName = "reloadHomeModules" | "runTestSuite";

export type ValidationOutcome =
  | { kind: "not-validation" }
  | { kind: "semantic"; toolName: ValidationToolName; success: boolean; structuredContent: unknown };

export function isDefinitionTool(name: string): boolean {
  return name === "getDefinition";
}

export function classifyValidationTool(name: string): ValidationToolName | undefined {
  if (name === "reloadHomeModules" || name === "runTestSuite") {
    return name;
  }
  return undefined;
}

export function decodeStructuredToolResult(value: unknown): LoreStructuredToolResult {
  if (!value || typeof value !== "object") {
    throw new LoreProtocolError("Lore structured tool call returned a non-object result");
  }
  const obj = value as Record<string, unknown>;
  if (!Array.isArray(obj.content)) {
    throw new LoreProtocolError("Lore structured tool call result is missing content");
  }
  return {
    content: obj.content as McpContent[],
    isError: typeof obj.isError === "boolean" ? obj.isError : undefined,
    structuredContent: obj.structuredContent,
  };
}

export function decodeValidationOutcome(
  toolName: string,
  result: LoreStructuredToolResult,
): ValidationOutcome {
  const validationTool = classifyValidationTool(toolName);
  if (!validationTool) {
    return { kind: "not-validation" };
  }
  if (result.structuredContent === undefined || result.structuredContent === null) {
    throw new LoreProtocolError(`${toolName} did not include structuredContent`);
  }
  return {
    kind: "semantic",
    toolName: validationTool,
    success: decodeValidationSuccess(validationTool, result.structuredContent),
    structuredContent: result.structuredContent,
  };
}

export function decodeValidationSuccess(toolName: ValidationToolName, structuredContent: unknown): boolean {
  if (!structuredContent || typeof structuredContent !== "object") {
    throw new LoreProtocolError(`${toolName} structuredContent must be an object`);
  }
  const obj = structuredContent as Record<string, unknown>;

  if (typeof obj.success === "boolean") {
    return obj.success;
  }

  if (typeof obj.status === "string") {
    return successFromLegacyStatus(toolName, obj.status);
  }

  throw new LoreProtocolError(`${toolName} structuredContent is missing boolean success`);
}

function successFromLegacyStatus(toolName: ValidationToolName, status: string): boolean {
  if (toolName === "reloadHomeModules") {
    if (status === "success") return true;
    if (status === "compilation-failure") return false;
  }
  if (toolName === "runTestSuite") {
    if (status === "tests-passed" || status === "no-tests") return true;
    if (
      status === "tests-failed" ||
      status === "compilation-failure" ||
      status === "invalid-arguments" ||
      status === "blocked"
    ) {
      return false;
    }
  }
  throw new LoreProtocolError(`${toolName} structuredContent has unknown status ${JSON.stringify(status)}`);
}

export function renderedText(result: LoreStructuredToolResult): string {
  return result.content
    .map((part) => {
      if (typeof part.text === "string") {
        return part.text;
      }
      return "";
    })
    .filter(Boolean)
    .join("\n");
}
