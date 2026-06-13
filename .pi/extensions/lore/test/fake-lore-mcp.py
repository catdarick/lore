#!/usr/bin/env python3
import json
import sys
import time

cache = []

tools = [
    {
        "name": "getDefinition",
        "description": "Return definitions",
        "inputSchema": {"type": "object", "properties": {"symbols": {"type": "array"}}},
    },
    {
        "name": "reloadHomeModules",
        "description": "Reload modules",
        "inputSchema": {
            "type": "object",
            "properties": {"success": {"type": "boolean"}, "sleepMs": {"type": "number"}},
        },
    },
    {
        "name": "runTestSuite",
        "description": "Run tests",
        "inputSchema": {
            "type": "object",
            "properties": {
                "success": {"type": ["boolean", "null"]},
                "testArgs": {"type": ["array", "null"], "items": {"type": "string"}},
            },
            "required": ["success", "testArgs"],
        },
    },
    {"name": "echo", "description": "Echo arguments", "inputSchema": {"type": "object"}},
]


def envelope(text, structured):
    return {"content": [{"type": "text", "text": text}], "isError": False, "structuredContent": structured}


def call_tool(name, args):
    global cache
    if args.get("malformedOutput"):
        print("not-json", flush=True)
        while True:
            time.sleep(60)
    if args.get("exit"):
        sys.exit(7)
    if isinstance(args.get("sleepMs"), (int, float)):
        time.sleep(args["sleepMs"] / 1000)
    if name == "getDefinition":
        symbols = [str(item) for item in args.get("symbols", ["unknown"])]
        for symbol in symbols:
            value = f"hash:{symbol}"
            if value not in cache:
                cache.append(value)
        cache = sorted(cache)
        return envelope(f"definition {','.join(symbols)}", {"tool": name, "symbols": symbols})
    if name == "reloadHomeModules":
        success = args.get("success") is not False
        if args.get("emptyContent"):
            return {"content": [], "isError": False, "structuredContent": {"tool": name, "success": success}}
        return envelope("reload ok" if success else "reload failed", {"tool": name, "success": success})
    if name == "runTestSuite":
        success = args.get("success") is not False
        return envelope("tests ok" if success else "tests failed", {"tool": name, "success": success, "invocation": args})
    return envelope("echo", {"tool": name, "args": args, "cache": cache})


def handle(method, params):
    global cache
    if method == "initialize":
        print("fake lore ready", file=sys.stderr, flush=True)
        return {"protocolVersion": "2024-11-05", "capabilities": {}}
    if method == "tools/list":
        return {"tools": tools}
    if method == "lore/knowledge/getCachedDefinitions":
        return {"hashes": cache}
    if method == "lore/knowledge/setCachedDefinitions":
        cache = sorted([str(item) for item in params["hashes"]])
        return {"cachedDefinitionCount": len(cache)}
    if method == "lore/tools/callStructured":
        return call_tool(params["name"], params.get("arguments") or {})
    raise Exception(f"unknown method {method}")


for line in sys.stdin:
    if not line.strip():
        continue
    request = json.loads(line)
    if "id" not in request:
        continue
    try:
        result = handle(request["method"], request.get("params"))
        print(json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": result}), flush=True)
    except Exception as exc:
        print(
            json.dumps({"jsonrpc": "2.0", "id": request["id"], "error": {"code": -32000, "message": str(exc)}}),
            flush=True,
        )
