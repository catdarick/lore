export class LoreInfrastructureError extends Error {
  readonly causeCode: string;

  constructor(causeCode: string, message: string) {
    super(message);
    this.name = "LoreInfrastructureError";
    this.causeCode = causeCode;
  }
}

export class LoreTimeoutError extends LoreInfrastructureError {
  constructor(message: string) {
    super("timeout", message);
    this.name = "LoreTimeoutError";
  }
}

export class LoreCancelledError extends LoreInfrastructureError {
  constructor(message: string) {
    super("cancelled", message);
    this.name = "LoreCancelledError";
  }
}

export class LoreProtocolError extends LoreInfrastructureError {
  readonly rpcCode?: number;

  constructor(message: string, rpcCode?: number) {
    super("protocol", message);
    this.name = "LoreProtocolError";
    this.rpcCode = rpcCode;
  }
}

export class LoreRemoteError extends Error {
  readonly rpcCode?: number;
  readonly data?: unknown;

  constructor(message: string, rpcCode?: number, data?: unknown) {
    super(message);
    this.name = "LoreRemoteError";
    this.rpcCode = rpcCode;
    this.data = data;
  }
}

export class LoreProcessError extends LoreInfrastructureError {
  constructor(message: string) {
    super("process", message);
    this.name = "LoreProcessError";
  }
}
