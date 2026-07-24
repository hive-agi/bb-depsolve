# bb-depsolve

Monorepo dependency management for Clojure/Babashka workspaces.

Sync internal git deps, upgrade mvn versions, and visualize dependency drift — all from one CLI.

## Install

### Via bbin

```bash
bbin install io.github.hive-agi/bb-depsolve
```

### As a workspace dependency

Add to your `bb.edn`:

```clojure
{:deps {io.github.hive-agi/bb-depsolve {:local/root "bb-depsolve"}}
 :tasks
 {sync {:task (exec 'bb-depsolve.cli/-main) :extra-args ["sync" "--org" "your-org"]}
  upgrade {:task (exec 'bb-depsolve.cli/-main) :extra-args ["upgrade"]}
  lint {:task (exec 'bb-depsolve.cli/-main) :extra-args ["lint"]}
  bump {:task (exec 'bb-depsolve.cli/-main) :extra-args ["bump"]}
  deps-report {:task (exec 'bb-depsolve.cli/-main) :extra-args ["report"]}}}
```

## Commands

### `sync` — Sync internal deps (git + mvn coords)

Finds all `io.github.{org}/*` deps across your workspace and updates them to the latest tag.
Handles both coordinate styles:

- `:git/tag` + `:git/sha` — tag and sha are updated together
- `:mvn/version` — version is updated to the latest tag (without the `v` prefix)

```bash
bb -m bb-depsolve.cli sync --root . --org hive-agi
bb -m bb-depsolve.cli sync --root . --org hive-agi --apply  # write changes
```

### `upgrade` — Upgrade mvn dependencies

Checks Clojars and Maven Central for newer versions of all `:mvn/version` deps.

```bash
bb -m bb-depsolve.cli upgrade --root .
bb -m bb-depsolve.cli upgrade --root . --apply         # interactive selection
bb -m bb-depsolve.cli upgrade --root . --pre-release    # include alpha/beta/rc
```

### `lint` — Detect dep anti-patterns

Finds `:local/root` deps that should be converted to `:git/tag` or `:mvn/version` before publishing. Optionally auto-fixes by splitting into a `local.deps.edn` overlay.

```bash
bb-depsolve lint --root .
bb-depsolve lint --root . --fix  # auto-split :local/root into local.deps.edn
```

### `bump` — Bump VERSION, tag, push

Reads a project's `VERSION` file, increments the version, commits, tags, and pushes. Designed for pre-1.0 semver conventions.

```bash
bb-depsolve bump                          # minor (patch): 0.2.1 -> 0.2.2
bb-depsolve bump --major                  # major (minor): 0.2.1 -> 0.3.0
bb-depsolve bump --stable                 # stable (major): 0.2.1 -> 1.0.0
bb-depsolve bump --sync --org hive-agi    # bump + update downstream deps
```

| Flag | Effect | Example |
|------|--------|---------|
| _(default)_ | Bump patch | `0.2.1` → `0.2.2` |
| `--minor` | Bump patch | `0.2.1` → `0.2.2` |
| `--major` | Bump minor, zero patch | `0.2.1` → `0.3.0` |
| `--stable` | Bump major, zero rest | `0.2.1` → `1.0.0` |
| `--sync --org <name>` | After bump, run `sync --apply` on workspace | |

### `report` — Dependency matrix

Shows which libraries are shared across projects and highlights version drift.

```bash
bb-depsolve report --root .
```

### `graph` — Internal dependency DAG in release order

Builds the graph of which workspace projects depend on which, and partitions it into
release levels: every project at level *n* has all of its dependencies at levels below *n*.
Reports dependency cycles, projects blocked behind them, and internal coordinates pinned by
a bare `:git/sha` (which carry no comparable version).

Only top-level `:deps` constrain ordering — a coordinate that appears solely under an alias
or a `bb.edn` task is still tracked for rewriting, but does not force a release order.

```bash
bb-depsolve graph --root . --org hive-agi
bb-depsolve graph --root . --org hive-agi --format dot | dot -Tsvg -o deps.svg
bb-depsolve graph --root . --org hive-agi --format edn
```

### `impact` — Blast radius of one release

Answers "if I release X, what else has to go out?" — direct consumers, the full transitive
closure, and the wave order they must be released in.

```bash
bb-depsolve impact --lib hive-weave --root . --org hive-agi
```

### `cascade` — Plan a transitive release

The whole point: releasing `hive-weave` means its consumers must re-pin it and be released
too, then *their* consumers, and so on. `cascade` computes that as an ordered plan.

Seeds come from `--from`; without it, every project holding unpublished commits seeds the
cascade. Each wave lists the version each project moves to, the pins it must rewrite, and
the artifacts to wait for before the next wave starts.

```bash
bb-depsolve cascade --from hive-weave --root . --org hive-agi
bb-depsolve cascade --root . --org hive-agi              # auto-detect seeds
bb-depsolve cascade --from hive-weave --org hive-agi --no-wait
bb-depsolve cascade --from hive-weave --org hive-agi --format edn
```

Both release models are handled: **pinned** projects (a tracked `VERSION` file) get an
explicit bump, while **rolling** projects (version derived as `0.{minor}.{commit-count}`)
are released by the push itself, so no bump is planned for them.

When a project's `VERSION` file is behind versions its consumers already pin, the plan says
so and advances from the higher one rather than planning a downgrade.

This command plans only — it never writes. `--apply` is refused rather than silently
ignored.

## Options

| Flag | Default | Description |
|------|---------|-------------|
| `--root <dir>` | `.` | Workspace root directory |
| `--org <name>` | — | GitHub org for internal deps (required for `sync`) |
| `--skip-dirs <csv>` | `vendor,node_modules,.git,target,.cpcache,.lsp` | Directories to skip |
| `--depth <n>` | `1` | How deep to scan for dep files |
| `--apply` | `false` | Write changes (default: dry-run) |
| `--fix` | `false` | Auto-fix lint issues (split `:local/root` into `local.deps.edn`) |
| `--pre-release` | `false` | Include pre-release versions in `upgrade` |
| `--major` | `false` | Bump minor version (pre-1.0 major) |
| `--minor` | `false` | Bump patch version (explicit, same as default) |
| `--stable` | `false` | Bump major version (1.0 release) |
| `--sync` | `false` | After `bump`, run sync on workspace |
| `--from <csv>` | — | Seed projects for `cascade` (default: everything unpublished) |
| `--lib <name>` | — | Target project for `impact` |
| `--format <fmt>` | `text` | Output format: `text`, `edn`, or `dot` (`graph` only) |
| `--no-wait` | `false` | Plan without waiting for each wave's artifacts to publish |
| `--await-timeout <ms>` | `900000` | Per-wave ceiling for waiting on published artifacts |

## TUI

When running in an interactive terminal with [gum](https://github.com/charmbracelet/gum) available, `upgrade --apply` shows an interactive multi-select for choosing which deps to upgrade. Falls back to plain text in non-TTY environments.

## License

Copyright (c) 2024-2026 hive-agi contributors

EPL-2.0 — see [LICENSE](LICENSE).
