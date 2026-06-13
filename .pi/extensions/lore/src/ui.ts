import type { PiHost, RecoveryDiff, RecoveryObligations, RecoveryState, RecoveryTokenMetrics } from "./types.ts";

const statusKey = "lore-recovery";
export const recoverySummaryCustomType = "lore-recovery-summary";

export class LoreRecoveryUi {
  private readonly host: PiHost;

  constructor(host: PiHost) {
    this.host = host;
  }

  async showActiveStatus(obligations: RecoveryObligations): Promise<void> {
    const text = statusText(obligations);
    if (text) {
      await this.host.setStatus?.(statusKey, text, { tone: "warning" });
      return;
    }
    await this.host.clearStatus?.(statusKey);
  }

  async showFinalizingStatus(): Promise<void> {
    await this.host.setStatus?.(statusKey, "⚠ Lore recovery: finalization pending", { tone: "warning" });
  }

  async clearStatus(): Promise<void> {
    await this.host.clearStatus?.(statusKey);
  }

  async showInfrastructureError(message: string): Promise<void> {
    await this.host.notify?.(message, { tone: "error" });
  }

  async appendStartSeparator(_recoveryId: string, reason: string): Promise<void> {
    await this.host.notify?.(`Recovery started: ${reason}`);
  }

  async appendContextMarker(recovery: Exclude<RecoveryState, { phase: "inactive" }>): Promise<void> {
    if (!this.host.sendMessage) {
      throw new Error("Lore recovery marker delivery requires host.sendMessage");
    }
    await this.host.sendMessage(
      {
        customType: "lore-recovery-context-marker",
        display: false,
        content: recovery.contextMarker,
        details: {
          tone: "warning",
          loreExtension: {
            kind: "recoveryState",
            state: recovery,
          },
        },
      },
      { deliverAs: "steer" },
    );
  }

  async appendCompletionMessage(
    recoveryId: string,
    summary: string,
    contextReplacement: string,
  ): Promise<"appended" | "already-present"> {
    if (!this.host.appendDisplayMessageImmediately) {
      throw new Error("Lore recovery completion delivery requires host.appendDisplayMessageImmediately");
    }
    return this.host.appendDisplayMessageImmediately({
      customType: recoverySummaryCustomType,
      display: true,
      content: contextReplacement,
      details: {
        recoveryId,
        summary,
        hiddenFromModel: true,
      },
    });
  }

  async notifyRecoveryCompleted(diff: RecoveryDiff, tokenMetrics?: RecoveryTokenMetrics): Promise<void> {
    const tokenText = tokenMetrics
      ? ` Recovery context: ~${tokenMetrics.originalRecoveryTokens} tokens -> summary: ~${tokenMetrics.summaryReplacementTokens} tokens.`
      : "";
    await this.host.notify?.(
      `Recovery completed: ${diff.stats.filesChanged} files changed, +${diff.stats.additions}/-${diff.stats.deletions}.${tokenText}`,
    );
  }
}

export function statusText(obligations: RecoveryObligations): string | undefined {
  if (obligations.compilationPending && obligations.testsPending) {
    return "⚠ Lore recovery: compilation + tests pending";
  }
  if (obligations.compilationPending) {
    return "⚠ Lore recovery: compilation pending";
  }
  if (obligations.testsPending) {
    return "⚠ Lore recovery: tests pending";
  }
  return undefined;
}
