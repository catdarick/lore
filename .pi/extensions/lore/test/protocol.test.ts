import assert from "node:assert/strict";
import { test } from "node:test";
import { decodeValidationSuccess } from "../src/index.ts";

test("decodes boolean structured validation success", () => {
  assert.equal(decodeValidationSuccess("reloadHomeModules", { success: true }), true);
  assert.equal(decodeValidationSuccess("runTestSuite", { success: false }), false);
});

test("decodes local Lore structured status in the adapter only", () => {
  assert.equal(decodeValidationSuccess("reloadHomeModules", { status: "success" }), true);
  assert.equal(decodeValidationSuccess("reloadHomeModules", { status: "compilation-failure" }), false);
  assert.equal(decodeValidationSuccess("runTestSuite", { status: "tests-passed" }), true);
  assert.equal(decodeValidationSuccess("runTestSuite", { status: "tests-failed" }), false);
});
