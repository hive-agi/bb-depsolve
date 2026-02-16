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

### `sync` — Sync internal git deps

Finds all `io.github.{org}/*` git deps across your workspace and updates them to the latest tag+sha.

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

## TUI

When running in an interactive terminal with [gum](https://github.com/charmbracelet/gum) available, `upgrade --apply` shows an interactive multi-select for choosing which deps to upgrade. Falls back to plain text in non-TTY environments.

## License

Copyright (c) 2024-2026 hive-agi contributors

EPL-2.0 — see [LICENSE](LICENSE).
