# Custom command tools

A project can expose a trusted shell command as an MCP tool through `lore.yaml`. Each configured tool gets its own name, description, and input schema.

**Benefit over plain command output:** A custom tool can return the exact project-specific operation an agent needs, without teaching the agent a long command sequence.

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
```

The generated input is:

```json
{ "directory": "content/drafts" }
```

## What the agent receives

The result contains the process exit code, stdout, and stderr. A custom tool named `runTestSuite` can replace Lore's built-in test runner while preserving a structured pass or fail status.

Custom commands execute through the shell. Keep single-quote mode unless raw shell syntax is intentional, and enable these tools only for trusted projects.
