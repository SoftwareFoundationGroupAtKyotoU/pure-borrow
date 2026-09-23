# Agent plugins for Linear Haskell and Pure Borrow

This directory ships two agent plugins, each containing one [Agent Skill](https://agentskills.io/specification), so that coding agents write idiomatic, sound Linear Haskell and pure-borrow code.

| Plugin | Skill | Use it for |
| --- | --- | --- |
| `linear-haskell` | `linear-haskell` | Linear Haskell in general: GHC `LinearTypes` and `linear-base` on GHC 9.10.3 and 9.12.4+. |
| `pure-borrow` | `pure-borrow` | The pure-borrow library: `BO`, lifetimes, `Mut`/`Share`/`Lend`, parallelism. Builds on `linear-haskell`. |

Each plugin follows the [Agent Plugins 1.0.0](https://agent-plugins.org/specification) layout: a portable `plugin.json` at the plugin root and the skill under `skills/<name>/`.
The repository root carries one marketplace file per tool, so everything installs straight from this repository:

```text
.claude-plugin/marketplace.json     Claude Code; also read by `npx skills`, and by Codex if the next file is absent
.agents/plugins/marketplace.json    Codex
.cursor-plugin/marketplace.json     Cursor
agent-plugins/<plugin>/plugin.json
agent-plugins/<plugin>/LICENSE
agent-plugins/<plugin>/skills/<plugin>/SKILL.md
agent-plugins/<plugin>/skills/<plugin>/references/*.md
```

## Installation

### Claude Code

```text
/plugin marketplace add SoftwareFoundationGroupAtKyotoU/pure-borrow
/plugin install pure-borrow@pure-borrow
```

`pure-borrow` declares a dependency on `linear-haskell`, which Claude Code installs automatically.
Install `linear-haskell@pure-borrow` alone if you only write Linear Haskell without the library.
The `owner/repo` shorthand clones over SSH; without a GitHub SSH key, use `/plugin marketplace add https://github.com/SoftwareFoundationGroupAtKyotoU/pure-borrow.git` instead (or set `CLAUDE_CODE_PLUGIN_PREFER_HTTPS=1`).
From a shell, the same steps are `claude plugin marketplace add …` and `claude plugin install pure-borrow@pure-borrow`.

### Codex (CLI and desktop app)

```sh
codex plugin marketplace add SoftwareFoundationGroupAtKyotoU/pure-borrow
codex plugin add linear-haskell@pure-borrow
codex plugin add pure-borrow@pure-borrow
```

Codex does not resolve dependencies between plugins, so install both.
Inside a session, `/plugins` offers the same.
The Codex IDE extension does not support plugins; install the skills directly instead (see below).

### Cursor

Cursor does not resolve dependencies between plugins, so install both `linear-haskell` and `pure-borrow` with whichever route fits:

- **Cursor CLI**: register the marketplace with `agent plugin marketplace add https://github.com/SoftwareFoundationGroupAtKyotoU/pure-borrow.git` (or `/plugin marketplace add <url>` in a session), then install both plugins from `/plugin`.
- **Teams and Enterprise**: in **Dashboard → Plugins & MCPs**, add a team marketplace with **Import from Repo** and this repository's URL, then install both plugins from **Customize**.
- **Local copy**: copy `agent-plugins/linear-haskell` and `agent-plugins/pure-borrow` into `~/.cursor/plugins/local/` (copies, not symlinks to elsewhere) and reload the window.
- **Skills only**: `npx skills add SoftwareFoundationGroupAtKyotoU/pure-borrow --skill '*' -a cursor` (see below).

### Skills only, for any Agent Skills–compatible agent

```sh
npx skills add SoftwareFoundationGroupAtKyotoU/pure-borrow --skill '*' -a claude-code -a codex -a cursor
```

Omit `--skill '*'` to choose skills interactively, pass other `-a <agent>` names for other agents, and add `-y` to skip the prompts.

## Using the skills

The skills activate automatically when an agent works on Linear Haskell or pure-borrow code, or when you mention them.
In Claude Code, plugin skills are namespaced, e.g. `/pure-borrow:pure-borrow`.
The `pure-borrow` skill tells the agent to load `linear-haskell` first, so keep both installed.

## Maintenance

- `SKILL.md` frontmatter uses only the Agent Skills fields (`name`, `description`, `license`, `compatibility`, `metadata`, `allowed-tools`), and `name` must equal the skill's directory name.
- `plugin.json` has a closed schema: tool-specific settings (such as Claude Code's `dependencies`) belong in the marketplace files, never in `plugin.json`, and nothing inside a plugin may point outside its directory; that is why each plugin carries its own copy of `LICENSE`.
- The entries in `.agents/plugins/marketplace.json` repeat the plugin metadata on purpose: Codex releases before Agent Plugins support (0.146) build their manifest from them, while newer ones read `plugin.json`.
- Bump `version` in `agent-plugins/*/plugin.json` and in all three marketplace files together whenever a skill changes; Claude Code and Codex only refresh an installed plugin when its version changes.
- Keep the skills in sync with the library: when an API changes, update the affected signatures and examples, and recompile the examples on every supported GHC.
