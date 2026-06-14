import type { LoreClient } from "./mcp-client.ts";
import { normalizeSnapshot, sameSnapshot, type SessionStateStore } from "./session-state.ts";
import type { KnowledgeSnapshot } from "./types.ts";

export class KnowledgeSynchronizer {
  private readonly client: LoreClient;
  private readonly store: SessionStateStore;
  private readonly branchAnchor: () => string | undefined;
  private pending:
    | {
        base: KnowledgeSnapshot;
        snapshot: KnowledgeSnapshot;
        anchor?: string;
      }
    | undefined;

  constructor(client: LoreClient, store: SessionStateStore, branchAnchor: () => string | undefined = () => undefined) {
    this.client = client;
    this.store = store;
    this.branchAnchor = branchAnchor;
  }

  async restoreActiveBranch(): Promise<void> {
    const state = await this.store.reloadFromBranch();
    const snapshot = this.snapshotForCurrentBranch(state.knowledge);
    await this.client.setCachedDefinitions(snapshot.hashes);
  }

  async captureIfChanged(): Promise<KnowledgeSnapshot | undefined> {
    const hashes = await this.client.getCachedDefinitions();
    const snapshot = normalizeSnapshot({ hashes });
    const current = this.pending?.snapshot ?? this.store.current().knowledge;
    if (sameSnapshot(snapshot, current)) {
      return undefined;
    }
    this.pending = {
      base: normalizeSnapshot(this.store.current().knowledge),
      snapshot,
      anchor: this.branchAnchor(),
    };
    return snapshot;
  }

  async reset(): Promise<void> {
    const snapshot = { hashes: [] };
    this.pending = undefined;
    await this.store.persist({ kind: "knowledgeReset", snapshot });
    await this.client.setCachedDefinitions([]);
  }

  async synchronizeEmpty(): Promise<void> {
    this.pending = undefined;
    await this.client.setCachedDefinitions([]);
  }

  private snapshotForCurrentBranch(branchSnapshot: KnowledgeSnapshot): KnowledgeSnapshot {
    if (!this.pending) {
      return branchSnapshot;
    }
    const anchor = this.branchAnchor();
    if (this.pending.anchor !== anchor) {
      this.pending = undefined;
      return branchSnapshot;
    }
    if (sameSnapshot(branchSnapshot, this.pending.snapshot)) {
      this.pending = undefined;
      return branchSnapshot;
    }
    if (sameSnapshot(branchSnapshot, this.pending.base)) {
      return this.pending.snapshot;
    }
    this.pending = undefined;
    return branchSnapshot;
  }
}
