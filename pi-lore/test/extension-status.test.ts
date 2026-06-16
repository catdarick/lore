import assert from "node:assert/strict";
import { test } from "node:test";
import {
  loreExtensionStatusText,
  renderLoreExtensionStatus,
  showLoreExtensionStatus,
  type LoreExtensionStatus,
} from "../src/extension-status.ts";
import { adaptPiHost, type ExtensionContext } from "../src/pi-adapter.ts";

const expectedRoles: Record<LoreExtensionStatus, string> = {
  preparing: "accent",
  setupRequired: "warning",
  downloading: "accent",
  starting: "accent",
  active: "text",
  disabled: "text",
  paused: "warning",
  unavailable: "error",
};

test("Lore status rendering colors the label and state through the Pi theme", () => {
  const theme = {
    fg(role: string, text: string) {
      return `<${role}>${text}</${role}>`;
    },
  };

  for (const [status, role] of Object.entries(expectedRoles) as Array<[LoreExtensionStatus, string]>) {
    const plain = loreExtensionStatusText(status);
    const stateText = plain.slice("Lore: ".length);
    assert.equal(
      renderLoreExtensionStatus(status, theme),
      `<accent>Lore:</accent> <${role}>${stateText}</${role}>`,
    );
  }
});

test("Lore status rendering falls back to ANSI colors", () => {
  assert.equal(
    renderLoreExtensionStatus("starting"),
    "\u001b[34mLore:\u001b[39m \u001b[36mstarting…\u001b[39m",
  );
  assert.equal(
    renderLoreExtensionStatus("unavailable"),
    "\u001b[34mLore:\u001b[39m \u001b[31munavailable\u001b[39m",
  );
  assert.equal(
    renderLoreExtensionStatus("disabled"),
    "\u001b[34mLore:\u001b[39m \u001b[97mdisabled\u001b[39m",
  );
});

test("Pi host adapter applies semantic Lore status colors", async () => {
  let renderedStatus: string | undefined;
  const context: ExtensionContext = {
    sessionManager: {
      getBranch: () => [],
      getLeafId: () => null,
    },
    ui: {
      notify() {},
      setStatus(_key, text) {
        renderedStatus = text;
      },
      theme: {
        fg(role: string, text: string) {
          return `[${role}:${text}]`;
        },
      },
    },
  };
  const host = adaptPiHost({}, {
    getCurrentContext: () => context,
    setCurrentContext() {},
    async appendDisplayMessageImmediately() {},
  });

  await showLoreExtensionStatus(host, "setupRequired");
  assert.equal(renderedStatus, "[accent:Lore:] [warning:setup required]");
});
