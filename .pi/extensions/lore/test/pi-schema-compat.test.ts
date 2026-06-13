import assert from "node:assert/strict";
import { test } from "node:test";
import { adaptMcpSchemaForPiNullableRequiredBug } from "../src/pi-schema-compat.ts";

test("adapts required nullable object properties recursively", () => {
  const schema = {
    type: "object",
    required: ["outer"],
    properties: {
      outer: {
        type: "object",
        required: ["inner"],
        properties: {
          inner: { type: ["string", "null"], description: "nullable required" },
        },
      },
    },
  };
  assert.deepEqual(adaptMcpSchemaForPiNullableRequiredBug(schema), {
    type: "object",
    required: ["outer"],
    properties: {
      outer: {
        type: "object",
        properties: {
          inner: { type: "string", description: "nullable required" },
        },
      },
    },
  });
});

test("adapts array items, additionalProperties, and schema composition", () => {
  const schema = {
    type: "object",
    properties: {
      list: {
        type: "array",
        items: {
          type: "object",
          required: ["item"],
          properties: { item: { type: ["number", "null"] } },
        },
      },
      dictionary: {
        type: "object",
        additionalProperties: {
          type: "object",
          required: ["value"],
          properties: { value: { type: ["boolean", "null"] } },
        },
      },
    },
    allOf: [{ type: "object", required: ["a"], properties: { a: { type: ["string", "null"] } } }],
    anyOf: [{ type: "object", required: ["b"], properties: { b: { type: ["string", "null"] } } }],
    oneOf: [{ type: "object", required: ["c"], properties: { c: { type: ["string", "null"] } } }],
  };
  const adapted = adaptMcpSchemaForPiNullableRequiredBug(schema);
  assert.equal((adapted.properties.list.items.properties.item as { type: unknown }).type, "number");
  assert.equal(adapted.properties.list.items.required, undefined);
  assert.equal((adapted.properties.dictionary.additionalProperties.properties.value as { type: unknown }).type, "boolean");
  assert.equal(adapted.properties.dictionary.additionalProperties.required, undefined);
  assert.equal((adapted.allOf[0].properties.a as { type: unknown }).type, "string");
  assert.equal(adapted.allOf[0].required, undefined);
  assert.equal((adapted.anyOf[0].properties.b as { type: unknown }).type, "string");
  assert.equal(adapted.anyOf[0].required, undefined);
  assert.equal((adapted.oneOf[0].properties.c as { type: unknown }).type, "string");
  assert.equal(adapted.oneOf[0].required, undefined);
});

test("leaves unrelated schemas structurally equivalent", () => {
  const schema = {
    type: "object",
    required: ["name"],
    properties: {
      name: { type: "string", description: "required string" },
      maybe: { type: ["string", "null"], default: null },
    },
    additionalProperties: false,
  };
  assert.deepEqual(adaptMcpSchemaForPiNullableRequiredBug(schema), schema);
});
