# `runTestSuite`

Compile the project, then run its Cabal or Stack test components.

**Benefit over plain command output:** The result summarizes component outcomes and returns focused diagnostics instead of raw build logs.

This tool is disabled by default because tests can be expensive and can execute project code.

## Typical MCP input

```json
{
  "package": "blog",
  "testArgs": "--match \"publishes a valid article\""
}
```

Both fields are optional. `package` limits execution to one discovered package. Lore parses `testArgs` and appends them to configured default arguments.

## What the agent receives

The structured status distinguishes compilation failure, environment failure, invalid arguments, no tests, passed tests, and failed tests. The text result includes per-component pass, setup failure, or execution failure details.

A project can replace the built-in runner with a custom command. In that case, exit code `0` means success and the result also contains stdout and stderr.
