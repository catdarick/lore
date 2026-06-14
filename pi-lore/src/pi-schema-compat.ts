import type { JsonObject } from "./types.ts";

export function adaptMcpSchemaForPiNullableRequiredBug(inputSchema: JsonObject): JsonObject {
  const schema = structuredClone(inputSchema);
  if (isJsonObject(schema)) {
    adaptSchemaObject(schema);
  }
  return schema;
}

function adaptSchemaObject(schema: Record<string, unknown>): void {
  adaptNestedSchemas(schema);

  const properties = getObjectRecord(schema.properties);
  if (!properties) {
    return;
  }

  const requiredNames = getStringArray(schema.required);
  const nullableRequired = new Set(requiredNames.filter((name) => schemaAllowsNull(properties[name])));
  const filteredRequired = requiredNames.filter((name) => !nullableRequired.has(name));

  if (filteredRequired.length > 0) {
    schema.required = filteredRequired;
  } else if (requiredNames.length > 0) {
    delete schema.required;
  }

  for (const propertyName of nullableRequired) {
    const propertySchema = properties[propertyName];
    if (isJsonObject(propertySchema)) {
      removeNullFromTypeUnion(propertySchema);
    }
  }
}

function adaptNestedSchemas(schema: Record<string, unknown>): void {
  const properties = getObjectRecord(schema.properties);
  if (properties) {
    for (const propertySchema of Object.values(properties)) {
      if (isJsonObject(propertySchema)) {
        adaptSchemaObject(propertySchema);
      }
    }
  }

  const items = schema.items;
  if (Array.isArray(items)) {
    for (const itemSchema of items) {
      if (isJsonObject(itemSchema)) {
        adaptSchemaObject(itemSchema);
      }
    }
  } else if (isJsonObject(items)) {
    adaptSchemaObject(items);
  }

  const additionalProperties = schema.additionalProperties;
  if (isJsonObject(additionalProperties)) {
    adaptSchemaObject(additionalProperties);
  }

  for (const unionKey of ["allOf", "anyOf", "oneOf"] as const) {
    const unionSchemas = schema[unionKey];
    if (!Array.isArray(unionSchemas)) {
      continue;
    }
    for (const unionSchema of unionSchemas) {
      if (isJsonObject(unionSchema)) {
        adaptSchemaObject(unionSchema);
      }
    }
  }
}

function schemaAllowsNull(value: unknown): boolean {
  if (!isJsonObject(value)) {
    return false;
  }
  const type = value.type;
  return type === "null" || (Array.isArray(type) && type.includes("null"));
}

function removeNullFromTypeUnion(schema: Record<string, unknown>): void {
  const type = schema.type;
  if (type === "null") {
    delete schema.type;
    return;
  }
  if (!Array.isArray(type)) {
    return;
  }
  const filtered = type.filter((entry): entry is string => typeof entry === "string" && entry !== "null");
  if (filtered.length === 0) {
    delete schema.type;
  } else {
    schema.type = filtered.length === 1 ? filtered[0] : filtered;
  }
}

function getStringArray(value: unknown): string[] {
  return Array.isArray(value) ? value.filter((entry): entry is string => typeof entry === "string") : [];
}

function getObjectRecord(value: unknown): Record<string, unknown> | undefined {
  return isJsonObject(value) ? value : undefined;
}

function isJsonObject(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}
