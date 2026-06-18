# lore-cli

Terminal frontend for Lore tools.

Use `lore-cli` when you want compiler-aware project inspection from a shell, script, or CI job. It can run one command and exit, or start an interactive prompt.

## Build and run

Build the CLI with the same full GHC version as the project being inspected:

```bash
cabal build exe:lore-cli
cabal list-bin exe:lore-cli
```

Run one command:

```bash
cabal run lore-cli -- discover-project
cabal run lore-cli -- reload
cabal run lore-cli -- search-symbols loadProject
```

Run the interactive prompt:

```bash
cabal run lore-cli
```

Markdown is the default output format. Use JSON for scripts:

```bash
cabal run lore-cli -- --format json discover-project
```

## Compilation bottleneck analysis

`analyze-compilation-bottlenecks` builds a home-module dependency graph for each discovered component and reports structural limits on parallel compilation.

```bash
lore-cli analyze-compilation-bottlenecks --jobs 32 --limit 10
```

Aliases:

```bash
lore-cli module-bottlenecks
lore-cli compile-bottlenecks
```

### What it reports

The output is organized by component. Each component gets its own graph and stats:

- **Home modules** — modules in that component graph.
- **Home-module imports** — imports between modules in that same component graph.
- **Timing coverage** — modules with loaded `.dump-timings` samples and timing file count.
- **Jobs model** — the `-jN` parallelism level being modeled.
- **Total work** — sum of module costs. Without timings, each module is `1 module-unit`.
- **Critical path lower bound** — longest dependency chain in the component.
- **Worker-capacity lower bound** — `total work / jobs`; this is the lower bound imposed by the selected `-jN` worker count.
- **Estimated lower bound** — `max(critical path, total work / jobs)`, using the current structural/timing model.
- **Best possible average worker utilization** — theoretical upper bound from the dependency graph and cost model at the selected `-jN`.

The ranked modules printed under each component are ordered by a bottleneck score:

```text
moduleDuration * (1 + transitiveDependentCount) + directDependentCount
```

Without timing samples, `moduleDuration` is `1`, so the ranking is mostly “which modules block the most downstream modules.” With timing samples, slow upstream modules rank higher.

`--limit N` controls how many ranked modules are printed per component.

### Add GHC timing samples

Static graph analysis is useful, but real compile-time bottlenecks need per-module timing data. Generate timing files during a build:

```bash
stack build \
  --ghc-options="-fforce-recomp -ddump-to-file -ddump-timings -dumpdir .lore-timings"
```

or with Cabal:

```bash
cabal build all \
  --ghc-options="-fforce-recomp -ddump-to-file -ddump-timings -dumpdir .lore-timings"
```

Then pass the dump directory:

```bash
lore-cli analyze-compilation-bottlenecks \
  --jobs 32 \
  --timings .lore-timings \
  --limit 10
```

You can pass multiple timing paths either by repeating `--timings` or by using commas:

```bash
lore-cli analyze-compilation-bottlenecks \
  --timings package-a/.lore-timings \
  --timings package-b/.lore-timings

lore-cli analyze-compilation-bottlenecks \
  --timings package-a/.lore-timings,package-b/.lore-timings
```

Only files ending in `.dump-timings` or `.timings` are read. When GHC produces multiple timing files for the same module variant across repeated builds, the newest file for that variant is used. Distinct variants, such as `Foo.dump-timings` and `Foo.dyn.dump-timings`, are summed because they represent separate work done by the build.

### Interpreting the numbers

If a component has:

```text
Total work: 133.00 module-units
Critical path lower bound: 23.00 module-units
Worker-capacity lower bound at -j32: 4.16 module-units
Estimated lower bound at -j32: 23.00 module-units
Best possible average worker utilization at -j32: 18.1%
```

then `-j32` has enough worker capacity for the total work, but the dependency chain still forces at least `23` units of serial progress. The utilization bound is low because many workers would be idle even under an ideal scheduler.

Use the top-ranked modules in each component as decomposition candidates. They are usually upstream modules that many other modules wait for. Prefer changes that make the dependency graph narrower at the producer side:

- split stable types, small interfaces, or shared data declarations into a lighter upstream module;
- move heavy implementation, instances, generated code, or rarely used helpers into downstream modules;
- avoid making broadly imported modules depend on expensive implementation details;
- if timing samples show a top module is also slow, inspect its `.dump-timings` passes before changing consumers.

Do not treat the list as an automatic refactoring plan. A highly ranked module may be a good central abstraction. The useful signal is: “if this module is large or expensive, decomposing it can unlock more parallel compilation.”

When no timing samples are loaded, values are in `module-units`, not seconds. With full timing coverage, values are rendered in seconds.
