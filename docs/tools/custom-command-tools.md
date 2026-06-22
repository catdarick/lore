# Custom command tools

A project can expose a trusted shell command as an MCP tool through `lore.yaml`. Each configured tool gets its own name, description, and generated input schema. This creates a stable project command surface instead of relying on ad hoc terminal commands.

Custom command tools execute in the project environment. Enable them only for trusted projects.

## Example configuration

```yaml
mcp:
  custom-tools:
    - name: checkArticles
      description: Check article files for publishing errors
      command: ./scripts/check-articles @{directory}
      args:
        - name: directory
          description: Directory to check
          quote-mode: single
  tools:
    checkArticles: true
```

The generated tool input is:

```json
{ "directory": "content/drafts" }
```

## Placeholders and arguments

Placeholders use `@{argumentName}` syntax inside the configured command. Every placeholder must have a declared argument.

Each argument can be written as a short required-string form:

```yaml
args:
  - package
  - target
```

Or as an object with these options:

| Field | Default | Description |
| --- | --- | --- |
| `name` | required | Placeholder name used by `@{name}`. |
| `description` | unset | Description included in the generated MCP schema. |
| `nullable` | `false` | Allows the caller to pass `null`, which is substituted as an empty string. |
| `escape-quotes` | `false` | Escapes double quotes before insertion. |
| `quote-mode` | `single` | `single`, `double`, or `none`. `single` is the safest choice for a normal argument. |

Custom names cannot duplicate built-in tools, with one exception: a custom tool named `runTestSuite` replaces the built-in test runner.

## `runTestSuite` override

A project-specific test command can replace the built-in runner while preserving the structured success/failure result expected by clients:

```yaml
mcp:
  custom-tools:
    # Override the default runTestSuite tool for a better performance.
    - name: runTestSuite
      description: Run the test suite. Equivalent to `stack test {package} --cabal-verbosity 0 --ta "--format=failed-examples --no-color {testArgs}"`
      command: stack test @{package} --cabal-verbosity 0 --ta "--format=failed-examples --no-color @{testArgs}"
      args:
        - name: package
          description: Optional package name to run tests for. If not provided, all tests will be run.
          nullable: true
        - name: testArgs
          description: Optional additional arguments to pass to hspec. For example, you can pass `--match "test name"` to run only tests with names containing "test name".
          nullable: true
          escape-quotes: true
          quote-mode: none
```

Exit code `0` is reported as success; a non-zero exit code is reported as failure. The result also contains stdout and stderr.

## What the tool returns

The result contains the process exit code, stdout, and stderr. A custom `runTestSuite` result uses the same success/failure status convention as the built-in test tool, but command output is still command output; keep project scripts concise so the tool remains context-efficient.

## Security notes

Custom commands, `executeCode`, and `runTestSuite` execute code or commands from the project environment. Enable them only for projects and configuration you trust.

`quote-mode: none` is appropriate only when raw shell fragments are intentional. `quote-mode: single` is the safer default for normal argument substitution.
