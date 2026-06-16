import assert from "node:assert/strict";
import { test } from "node:test";
import { framedCustomUi, selectLoreMenu, styleUiSelection, styleUiTitle, type CustomUiComponent } from "../src/custom-ui.ts";
import { fakeKeybindings } from "./test-support.ts";

test("framed custom UI matches Pi menu structure", () => {
  const inner: CustomUiComponent = {
    render: () => ["Lore tools", "", "› [X] getDefinition"],
    handleInput() {},
    invalidate() {},
  };
  const framed = framedCustomUi(
    inner,
    { fg: (_role: string, text: string) => text },
    "navigate  toggle  cancel",
  );

  const lines = framed.render(48);
  assert.equal(lines[0], "─".repeat(48));
  assert.equal(lines.at(-1), "─".repeat(48));
  assert.equal(lines[1], "");
  assert.equal(lines[2], " Lore tools");
  assert.equal(lines.at(-3), " navigate  toggle  cancel");
  assert.equal(lines.at(-2), "");
});

test("framed custom UI delegates component lifecycle", () => {
  const received: string[] = [];
  let invalidated = false;
  const framed = framedCustomUi(
    {
      render: () => [],
      handleInput: (data) => received.push(data),
      invalidate: () => { invalidated = true; },
    },
    undefined,
    "footer",
  );

  framed.handleInput("x");
  framed.invalidate();
  assert.deepEqual(received, ["x"]);
  assert.equal(invalidated, true);
});

test("framed custom UI uses one border style for both separators", () => {
  const framed = framedCustomUi(
    {
      render: () => [],
      handleInput() {},
      invalidate() {},
    },
    { fg: (role: string, text: string) => `<${role}>${text}</${role}>` },
    "footer",
  );

  const lines = framed.render(12);
  assert.equal(lines[0], `<border>${"─".repeat(12)}</border>`);
  assert.equal(lines.at(-1), `<border>${"─".repeat(12)}</border>`);
});

test("framed custom UI preserves body styling and dims only the footer", () => {
  const framed = framedCustomUi(
    {
      render: () => ["Lore tools", "", "› [X] getDefinition"],
      handleInput() {},
      invalidate() {},
    },
    { fg: (role: string, text: string) => `<${role}>${text}</${role}>` },
    "navigate",
  );

  const lines = framed.render(40);
  assert.equal(lines[2], " Lore tools");
  assert.equal(lines[3], "");
  assert.equal(lines[4], " › [X] getDefinition");
  assert.equal(lines.at(-3), "<dim> navigate</dim>");
  assert.equal(lines[0].includes("<dim>"), false);
});

test("custom UI title and selected row use accent while unselected text stays native", () => {
  const theme = {
    fg: (role: string, text: string) => `<${role}>${text}</${role}>`,
    bold: (text: string) => `<bold>${text}</bold>`,
  };

  assert.equal(styleUiTitle(theme, "Lore tools"), "<accent><bold>Lore tools</bold></accent>");
  assert.equal(styleUiSelection(theme, "→ [X] getDefinition", true), "<accent>→ [X] getDefinition</accent>");
  assert.equal(styleUiSelection(theme, "  [ ] runTestSuite", false), "  [ ] runTestSuite");
});

test("selectLoreMenu prefers the shared custom renderer over native select", async () => {
  let nativeSelectCalled = false;
  let rendered: string[] = [];
  const selected = await selectLoreMenu(
    {
      select: async () => { nativeSelectCalled = true; return undefined; },
      custom: async <T>(factory: (...args: unknown[]) => unknown) => new Promise<T | undefined>((done) => {
        const component = factory(undefined, {
          fg: (role: string, text: string) => `<${role}>${text}</${role}>`,
          bold: (text: string) => `<bold>${text}</bold>`,
        }, fakeKeybindings(), done) as { render(width: number): string[]; handleInput(data: string): void };
        component.handleInput("down");
        rendered = component.render(60);
        component.handleInput("enter");
      }),
    },
    "Lore settings",
    ["Tools", "Recovery"],
  );

  assert.equal(selected, "Recovery");
  assert.equal(nativeSelectCalled, false);
  assert.equal(rendered.some((line) => line.includes("<accent><bold>Lore settings</bold></accent>")), true);
  assert.equal(rendered.some((line) => line.includes("<accent>→ Recovery</accent>")), true);
  assert.equal(rendered.some((line) => line.startsWith("<border>")), true);
});
