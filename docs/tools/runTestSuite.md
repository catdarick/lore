# `runTestSuite`

Compile the project, then run its Cabal or Stack test components. It provides structured validation status and focused output instead of raw test logs in context.

`runTestSuite` is disabled by default because it executes project tests and can be expensive. Enable it with `mcp.tools.runTestSuite: true` in `lore.yaml` or the matching environment override documented in the `lore-mcp` configuration guide.

Because it executes project code, enable this tool only for trusted projects and trusted test commands.

## Typical MCP input

```json
{
  "package": "blog",
  "testArgs": "--match \"publishes a valid article\""
}
```

Both fields are optional. `package` limits execution to one discovered package. Lore parses `testArgs` and appends them to configured default arguments.

## What the tool returns

The structured status distinguishes compilation failure, environment failure, invalid arguments, no tests, passed tests, and failed tests. The text result includes per-component pass, setup failure, or execution failure details.

A project can replace the built-in runner with a custom command. In that case, exit code `0` means success and the result also contains stdout and stderr. See [Custom command tools](custom-command-tools.md) for placeholder syntax, quoting rules, and the `runTestSuite` override example.
