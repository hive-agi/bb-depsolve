# bb-depsolve

<!-- hive-badges -->

[![Clojars Project](https://img.shields.io/clojars/v/io.github.hive-agi/bb-depsolve.svg)](https://clojars.org/io.github.hive-agi/bb-depsolve)
[![cljdoc](https://cljdoc.org/badge/io.github.hive-agi/bb-depsolve)](https://cljdoc.org/d/io.github.hive-agi/bb-depsolve/CURRENT)
[![release](https://github.com/hive-agi/bb-depsolve/actions/workflows/release.yml/badge.svg)](https://github.com/hive-agi/bb-depsolve/actions/workflows/release.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

<!-- /hive-badges -->

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
 {sync {:task (exec 'bb-depsolve.cli.main/-main) :extra-args ["sync" "--org" "your-org"]}
  upgrade {:task (exec 'bb-depsolve.cli.main/-main) :extra-args ["upgrade"]}
  lint {:task (exec 'bb-depsolve.cli.main/-main) :extra-args ["lint"]}
  bump {:task (exec 'bb-depsolve.cli.main/-main) :extra-args ["bump"]}
  deps-report {:task (exec 'bb-depsolve.cli.main/-main) :extra-args ["report"]}}}
```

## Commands

### `sync` — Sync internal deps (git + mvn coords)

Finds all `io.github.{org}/*` deps across your workspace and updates them to the latest tag.
Handles both coordinate styles:

- `:git/tag` + `:git/sha` — tag and sha are updated together
- `:mvn/version` — version is updated to the latest tag (without the `v` prefix)

```bash
bb -m bb-depsolve.cli.main sync --root . --org hive-agi
bb -m bb-depsolve.cli.main sync --root . --org hive-agi --apply  # write changes
```

A Maven version is resolved **per registry** (Clojars, Maven Central, and the private
registry the workspace declares), and every pin is chosen from the registries the
*pinning project itself* declares under `:mvn/repos`. A project with no private
registry only ever moves to what Clojars or Central serves; a newer version that exists
only privately is named on the row but never written:

```
  hive-mcp      io.github.hive-agi/hive-help   0.0.9 -> 0.1.0  (mvn via clojars)  hive-gitea has 0.1.1, not declared by this project
  hive-carto    io.github.hive-agi/hive-help   0.1.0 -> 0.1.1  (mvn via hive-gitea)
```

A pin no declared registry can serve at any version is held back and reported, and a
public lib whose private registry is ahead of the public one is reported as a registry
parity finding (see `parity`) with the forge sync that fixes it.

Only a registry that **answered** counts. A registry that did not (timeout, 5xx, or a
401 on the private one) is reported as unread, and every pin that depends on it is
held rather than moved: a blind read must never look like an absence. The `~/.m2`
cache is not consulted, since a stale cache standing in for a registry is exactly how a
plan full of downgrades gets written. For the same reason `--apply` refuses a plan in
which any row moves a pin down, unless `--allow-downgrade` names that rollback as
deliberate (for instance a public project pinned past what Clojars has).

### `parity` — Public libs whose private registry is ahead

Public consumers hold no credentials for the private registry, so a public lib
(`version.edn` `:publish :clojars`) must have its newest version on Clojars. When the
private registry is ahead, the repo was pushed to the private forge but not to GitHub,
whose release CI publishes to Clojars. `parity` finds those libs and names the push:

```bash
bb-depsolve parity --root . --org hive-agi
bb-depsolve parity --root . --org hive-agi --no-fail   # report without exiting 1
```

```
Registry parity: 1 finding(s)
  io.github.hive-agi/hive-help             clojars 0.1.0  <  hive-gitea 0.1.1
      public consumers cannot fetch 0.1.1. Sync the forges: push the private state to GitHub so its release CI publishes it publicly:
        git -C hive-help push origin HEAD:main
```

A lib is public when its `version.edn` says `:publish :clojars` **or** its checkout has a
github.com remote: the publish target follows the hosting, so a GitHub-hosted repo that
declares `:publish :none` is still flagged, with a note to fix the declaration.

Two further kinds are reported without failing the run: a lib declared private
(`:publish :gitea`) that a public registry lists, and a lib declared `:publish :none`
that any registry lists. Both mean the declared target and the artifacts disagree.
A registry that did not answer yields an `unread` finding, which fails the run too:
parity cannot be certified from a blind read.

### `upgrade` — Upgrade mvn dependencies

Checks Clojars and Maven Central for newer versions of all `:mvn/version` deps.

```bash
bb -m bb-depsolve.cli.main upgrade --root .
bb -m bb-depsolve.cli.main upgrade --root . --apply         # interactive selection
bb -m bb-depsolve.cli.main upgrade --root . --pre-release    # include alpha/beta/rc
```

### `lint` — Detect dep anti-patterns

Finds `:local/root` deps that should be converted to `:git/tag` or `:mvn/version` before publishing. Optionally auto-fixes by splitting into a `local.deps.edn` overlay.

```bash
bb-depsolve lint --root .
bb-depsolve lint --root . --fix  # auto-split :local/root into local.deps.edn
```

### `bump` — Bump VERSION, tag, push

Reads a project's `VERSION` file, increments the version, commits, tags, and pushes. Designed for pre-1.0 semver conventions.

Without `--apply` the command is a **dry run**: it prints the planned bump and touches nothing. Tagging and pushing mint a release, so they only happen when you ask for them.

```bash
bb-depsolve bump                          # dry run: 0.2.1 -> 0.2.2 (patch)
bb-depsolve bump --apply                  # patch:  0.2.1 -> 0.2.2, tag + push
bb-depsolve bump --minor --apply          # minor:  0.2.1 -> 0.3.0
bb-depsolve bump --stable --apply         # major:  0.2.1 -> 1.0.0
bb-depsolve bump --apply --sync --org hive-agi    # bump + update downstream deps
```

| Flag | Effect | Example |
|------|--------|---------|
| _(default)_ | Bump patch | `0.2.1` → `0.2.2` |
| `--minor` | Bump minor, zero patch | `0.2.1` → `0.3.0` |
| `--major` | Bump major, zero rest | `0.2.1` → `1.0.0` |
| `--stable` | Bump major, zero rest — the 1.0.0 promotion | `0.2.1` → `1.0.0` |
| `--apply` | Write VERSION, commit, tag, push (default: dry run) | |
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

An artifact counts as published only when every consumer the plan re-pins in a later
wave can fetch it: a consumer pinning by tag needs the tag, a consumer pinning by
`:mvn/version` needs the artifact on a registry its own `:mvn/repos` reach. A version
that exists only on the private registry does not release a wave whose consumers are
public projects.

`--no-wait` sets the plan's await mode to skip. A timeout is a loud failure naming
every lib that never published — never a silent continue:

```
await timed out after 900s (limit 900s)
  never published: io.github.hive-agi/hive-dsl
  re-run with --no-wait to plan past it.
```

Versions are resolved across GitHub tags, Clojars, Maven Central and the private Maven
registry the workspace declares (a `:mvn/repos` entry whose id has credentials in
`~/.m2/settings.xml`; `MAVEN_URL` + `MAVEN_USERNAME` + `MAVEN_TOKEN` override it).

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
| `--major` | `false` | Bump major version |
| `--minor` | `false` | Bump minor version |
| `--stable` | `false` | Bump major version (the 1.0 promotion) |
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
