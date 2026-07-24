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

Planning is the default and writes nothing. `--apply` executes the plan: each wave is
released, awaited, and only then does the next wave re-pin it.

```bash
bb-depsolve cascade --from hive-weave --org hive-agi --apply
```

#### Resuming an interrupted cascade

A cascade is not atomic — by the time a later wave fails, earlier waves are already
tagged and pushed. Execution therefore checkpoints the run to
`.bb-depsolve/cascade-run.edn` after every wave.

Re-running `--apply` picks the checkpoint up, skips every project it records as
released, and continues from the first unreleased step. Already-published versions
still feed the pin rewriting, so consumers re-pin against what actually shipped
rather than what was planned. A run that completes clears the checkpoint; a corrupt
one is ignored rather than aborting the run.

#### Waiting between waves

A wave's consumers cannot re-pin a dependency until that dependency's artifact is
actually resolvable, so every wave carries an await directive. The plan records it;
the executor performs it.

Waiting is visible. Each poll reports every lib in the wave and what it is waiting
for — an exact version for a pinned release, "anything newer" for a rolling one:

```
waiting on 2 of 3 artifact(s) — 34s elapsed
  ✔ io.github.hive-agi/hive-weave    published
  … io.github.hive-agi/hive-dsl      waiting = 0.5.9
  … io.github.hive-agi/hive-system   waiting > 0.2.11
```

On a terminal the block is redrawn in place; through a pipe each state change prints
one line. Polling backs off from 2s to a 15s ceiling, bounded by `--await-timeout`.

`--no-wait` sets the plan's await mode to skip. A timeout is a loud failure naming
every lib that never published — never a silent continue:

```
await timed out after 900s (limit 900s)
  never published: io.github.hive-agi/hive-dsl
  re-run with --no-wait to plan past it.
```

Versions are resolved across GitHub tags, Clojars, Maven Central and — when
`MAVEN_URL` is set — the private Gitea Maven registry.

#### What an interrupted cascade reports

Execution never discards what it finished. A failed run reports every wave that
completed, the outcome of every step (`released` / `sync-failed` / `release-failed`),
and the versions already published, so the remaining work is visible rather than
reconstructed by hand — and re-running `--apply` resumes from it.

Execution refuses to start on a plan carrying dependency cycles or unknown seeds
unless forced.

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
| `--force` | `false` | Execute a cascade despite cycles or unknown seeds |

## TUI

When running in an interactive terminal with [gum](https://github.com/charmbracelet/gum) available, `upgrade --apply` shows an interactive multi-select for choosing which deps to upgrade. Falls back to plain text in non-TTY environments.

## License

Copyright (c) 2024-2026 hive-agi contributors

EPL-2.0 — see [LICENSE](LICENSE).
