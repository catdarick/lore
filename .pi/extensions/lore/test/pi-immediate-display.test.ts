import assert from "node:assert/strict";
import { test } from "node:test";
import { installPiImmediateDisplayBridge } from "../src/pi-immediate-display.ts";
import type { PiCustomMessage } from "../src/types.ts";

function makeFakeAgentSessionClass() {
  const calls = {
    original: [] as Array<{ message: PiCustomMessage; options: unknown }>,
    appended: [] as Array<{ customType: string; content: unknown; display: boolean; details: unknown }>,
    emitted: [] as unknown[],
    steer: [] as unknown[],
    followUp: [] as unknown[],
  };

  class FakeAgentSession {
    sessionManager = {
      appendCustomMessageEntry: (
        customType: string,
        content: unknown,
        display: boolean,
        details: unknown,
      ) => {
        calls.appended.push({ customType, content, display, details });
      },
    };
    agent = {
      state: { messages: [] as unknown[] },
      steer: (message: unknown) => calls.steer.push(message),
      followUp: (message: unknown) => calls.followUp.push(message),
    };

    _emit(event: unknown): void {
      calls.emitted.push(event);
    }

    async sendCustomMessage(message: PiCustomMessage, options?: unknown): Promise<string> {
      calls.original.push({ message, options });
      return "delegated";
    }
  }

  return { FakeAgentSession, calls };
}

test("immediate display persists and emits without agent queue side effects", async () => {
  const { FakeAgentSession, calls } = makeFakeAgentSessionClass();
  await installPiImmediateDisplayBridge({
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
  });

  const session = new FakeAgentSession();
  const message = {
    customType: "lore-recovery-summary",
    content: "summary text",
    display: true,
    details: { recoveryId: "r1", hiddenFromModel: true },
  };
  await session.sendCustomMessage(message, { loreDisplayOnlyImmediate: true });

  assert.deepEqual(calls.appended, [
    {
      customType: "lore-recovery-summary",
      content: "summary text",
      display: true,
      details: { recoveryId: "r1", hiddenFromModel: true },
    },
  ]);
  assert.deepEqual(
    calls.emitted.map((event) => (event as { type?: string }).type),
    ["message_start", "message_end"],
  );
  assert.equal(calls.original.length, 0);
  assert.equal(calls.steer.length, 0);
  assert.equal(calls.followUp.length, 0);
  assert.equal(session.agent.state.messages.length, 0);
  assert.equal((calls.emitted[0] as { message?: { customType?: string } }).message?.customType, message.customType);
  assert.equal((calls.emitted[0] as { message?: { content?: unknown } }).message?.content, message.content);
  assert.equal((calls.emitted[0] as { message?: { display?: boolean } }).message?.display, true);
});

test("ordinary custom messages delegate to Pi unchanged", async () => {
  const { FakeAgentSession, calls } = makeFakeAgentSessionClass();
  await installPiImmediateDisplayBridge({
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
  });

  const session = new FakeAgentSession();
  const message = { customType: "ordinary", content: "hello", display: true };
  const options = { deliverAs: "steer" as const, triggerTurn: true };
  const result = await session.sendCustomMessage(message, options);

  assert.equal(result, "delegated");
  assert.deepEqual(calls.original, [{ message, options }]);
  assert.equal(calls.appended.length, 0);
  assert.equal(calls.emitted.length, 0);
});

test("bridge installation is idempotent for the same AgentSession class", async () => {
  const { FakeAgentSession, calls } = makeFakeAgentSessionClass();
  await installPiImmediateDisplayBridge({
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
  });
  await installPiImmediateDisplayBridge({
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
  });

  const session = new FakeAgentSession();
  const message = { customType: "ordinary", content: "hello", display: true };
  await session.sendCustomMessage(message, {});

  assert.equal(calls.original.length, 1);
});

test("unsupported runtime shapes fail clearly", async () => {
  class MissingSendCustomMessage {}
  await assert.rejects(
    () =>
      installPiImmediateDisplayBridge({
        agentSessionClass: MissingSendCustomMessage,
        piVersion: "0.79.1",
      }),
    /AgentSession\.sendCustomMessage is unavailable/,
  );

  const { FakeAgentSession } = makeFakeAgentSessionClass();
  await installPiImmediateDisplayBridge({
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
  });
  const session = new FakeAgentSession();
  (session as unknown as { sessionManager: unknown }).sessionManager = {};
  await assert.rejects(
    () => session.sendCustomMessage({ customType: "x", content: "x", display: true }, { loreDisplayOnlyImmediate: true }),
    /missing appendCustomMessageEntry or _emit/,
  );
});

test("unsupported Pi versions fail clearly", async () => {
  const { FakeAgentSession } = makeFakeAgentSessionClass();
  await assert.rejects(
    () =>
      installPiImmediateDisplayBridge({
        agentSessionClass: FakeAgentSession,
        piVersion: "0.80.0",
      }),
    /Supported Pi versions: >=0\.79\.1 <0\.80\.0/,
  );
});

test("appendImmediately calls Pi sendMessage with the branded private option", async () => {
  const { FakeAgentSession } = makeFakeAgentSessionClass();
  const session = new FakeAgentSession();
  const sent: Array<{ message: PiCustomMessage; options: unknown }> = [];
  const installer = await installPiImmediateDisplayBridge({
    sendMessage: (message, options) => {
      sent.push({ message, options });
      void session.sendCustomMessage(message, options).catch(() => {});
    },
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
  });
  const message = { customType: "lore-recovery-summary", content: "summary", display: true };

  await installer.appendImmediately(message);

  assert.equal(sent.length, 1);
  assert.equal(sent[0]?.message, message);
  assert.equal((sent[0]?.options as { loreDisplayOnlyImmediate?: unknown }).loreDisplayOnlyImmediate, true);
  assert.equal(typeof (sent[0]?.options as { loreImmediateDisplayRequestId?: unknown }).loreImmediateDisplayRequestId, "string");
});

test("appendImmediately resolves through Pi fire-and-forget sendMessage after patched append succeeds", async () => {
  const { FakeAgentSession, calls } = makeFakeAgentSessionClass();
  const session = new FakeAgentSession();
  const installer = await installPiImmediateDisplayBridge({
    sendMessage: (message, options) => {
      void session.sendCustomMessage(message, options).catch(() => {});
    },
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
    acknowledgementTimeoutMs: 50,
  });

  await installer.appendImmediately({ customType: "lore-recovery-summary", content: "summary", display: true });

  assert.equal(calls.appended.length, 1);
  assert.deepEqual(
    calls.emitted.map((event) => (event as { type?: string }).type),
    ["message_start", "message_end"],
  );
});

test("appendImmediately rejects when Pi fire-and-forget sendMessage swallows patched append failure", async () => {
  const { FakeAgentSession } = makeFakeAgentSessionClass();
  const session = new FakeAgentSession();
  (session as unknown as { sessionManager: { appendCustomMessageEntry: () => void } }).sessionManager = {
    appendCustomMessageEntry: () => {
      throw new Error("persist failed");
    },
  };
  const installer = await installPiImmediateDisplayBridge({
    sendMessage: (message, options) => {
      void session.sendCustomMessage(message, options).catch(() => {});
    },
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
    acknowledgementTimeoutMs: 50,
  });

  await assert.rejects(
    () => installer.appendImmediately({ customType: "lore-recovery-summary", content: "summary", display: true }),
    /persist failed/,
  );
});

test("appendImmediately times out if Pi never invokes patched sendCustomMessage", async () => {
  const { FakeAgentSession } = makeFakeAgentSessionClass();
  const installer = await installPiImmediateDisplayBridge({
    sendMessage: () => undefined,
    agentSessionClass: FakeAgentSession,
    piVersion: "0.79.1",
    acknowledgementTimeoutMs: 10,
  });

  await assert.rejects(
    () => installer.appendImmediately({ customType: "lore-recovery-summary", content: "summary", display: true }),
    /acknowledgement timed out/,
  );
});
