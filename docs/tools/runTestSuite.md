# `runTestSuite`

Compile the project, then run its Cabal or Stack test components. It provides structured validation status and focused output instead of raw test logs in context.


## Typical MCP input

```json
{
  "package": "blog",
  "testArgs": "--match \"publishes a valid article\""
}
```

Both fields are optional. `package` limits execution to one discovered package. Lore parses `testArgs` and appends them to configured default arguments.

## Reducing test-output noise

Configure framework-specific defaults so test runs return only useful failures. This keeps recovery sessions and normal validation from spending context on successful examples, ANSI color codes, or verbose progress output.

For Hspec projects, a good default is:

```yaml
session:
  default-test-args:
    - --format=failed-examples
    - --no-color
```

Per-call `testArgs` are appended after these defaults, so callers can still add filters such as `--match "publishes a valid article"`.

## What the tool returns

The structured status distinguishes compilation failure, environment failure, invalid arguments, no tests, passed tests, and failed tests. The text result includes per-component pass, setup failure, or execution failure details.

A project can replace the built-in runner with a custom command. In that case, exit code `0` means success and the result also contains stdout and stderr. See [Custom command tools](custom-command-tools.md) for placeholder syntax, quoting rules, and the `runTestSuite` override example.
