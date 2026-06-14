export type EstimatedUsageBucket = {
  calls: number;
  tokens: number;
};

export type LoreToolUsage = {
  toolName: string;
  main: EstimatedUsageBucket;
  summarizedRecovery: EstimatedUsageBucket;
};

export type LoreRecoveryUsage = {
  completedRecoveries: number;
  originalTokensSummarized: number;
  summaryReplacementTokens: number;
  estimatedReductionTokens: number;
  missingMetricsRecoveries: number;
};

export type LoreUsageStats = {
  tools: LoreToolUsage[];
  totals: {
    main: EstimatedUsageBucket;
    summarizedRecovery: EstimatedUsageBucket;
  };
  recovery: LoreRecoveryUsage;
  warnings: string[];
  estimated: true;
};
