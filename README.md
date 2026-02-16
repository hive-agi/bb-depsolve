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

### `report` — Dependency matrix

Shows which libraries are shared across projects and highlights version drift.

```bash
bb -m bb-depsolve.cli report --root .
```

## Options

| Flag | Default | Description |
|------|---------|-------------|
| `--root <dir>` | `.` | Workspace root directory |
| `--org <name>` | — | GitHub org for internal deps (required for `sync`) |
| `--skip-dirs <csv>` | `vendor,node_modules,.git,target,.cpcache,.lsp` | Directories to skip |
| `--depth <n>` | `1` | How deep to scan for dep files |
| `--apply` | `false` | Write changes (default: dry-run) |
| `--pre-release` | `false` | Include pre-release versions in `upgrade` |

## TUI

When running in an interactive terminal with [gum](https://github.com/charmbracelet/gum) available, `upgrade --apply` shows an interactive multi-select for choosing which deps to upgrade. Falls back to plain text in non-TTY environments.

## License

Copyright (c) 2024-2026 hive-agi contributors

EPL-2.0 — see [LICENSE](LICENSE).
