# agent-shell for Claude Code — design

## Goal

Replace running the Claude Code CLI raw inside eshell with
[`agent-shell`](https://github.com/xenodium/agent-shell), a native Emacs shell
that talks to LLM agents over the Agent Client Protocol (ACP). Target agent:
Anthropic's Claude Agent (formerly Claude Code), authenticated by reusing the
existing Claude Code login (no API key in the config).

## Scope

In scope:
- A new config module `modules/taomacs-ai.el` that installs and configures
  `agent-shell` for Claude, with a keybinding to launch it.
- Registering the module in `init.el`.

Out of scope:
- Other ACP agents (Gemini, Codex, etc.). The module is Claude-only for now;
  additional agents can be added later.
- Removing or changing the existing eshell/eat setup in `taomacs-shell.el`.
  agent-shell is additive; eshell stays as-is.

## Prerequisite (external, one-time, run in a terminal)

Install the Claude ACP adapter via mise's npm backend (the user already runs
`global-mise-mode`):

```bash
mise use -g "npm:@agentclientprotocol/claude-agent-acp"
```

This shims the `claude-agent-acp` binary onto `PATH`. Verify with
`which claude-agent-acp`. Because Emacs is launched from the GUI, the existing
`exec-path-from-shell` setup in `taomacs-dev.el` carries this `PATH` into Emacs.

Authentication is inherited from the existing Claude Code login — no API key or
token is stored anywhere in the Emacs config.

## Module: `modules/taomacs-ai.el`

```elisp
;;; taomacs-ai.el --- AI agents -*- lexical-binding: t -*-

;; agent-shell: native Emacs shell for LLM agents over ACP.
;; Claude uses the existing Claude Code login (no API key stored here).
;; Requires the `claude-agent-acp' adapter on PATH; install once with:
;;   mise use -g "npm:@agentclientprotocol/claude-agent-acp"
(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t))
  :bind
  (("C-c a" . agent-shell-anthropic-start-claude-code)))

(provide 'taomacs-ai)
;;; taomacs-ai.el ends here
```

Notes:
- `:ensure t` pulls `agent-shell` from MELPA, which auto-installs its
  dependencies `acp.el` and `shell-maker`.
- `agent-shell-anthropic-make-authentication` and
  `agent-shell-anthropic-start-claude-code` are provided by the
  `agent-shell-anthropic` sub-feature, which `agent-shell` loads. The `:custom`
  form runs after the package loads, so the constructor is available.

## init.el change

Add `taomacs-ai` to the `taomacs-modules` list, immediately after
`taomacs-shell`:

```elisp
    taomacs-shell
    taomacs-ai
    taomacs-dev
```

## Keybinding

`C-c a` → `agent-shell-anthropic-start-claude-code` ("a" for agent). Sits
alongside the existing `C-c t` eshell toggle. This is the single frequently-used
entry point; other agent-shell commands are left to `M-x`, consistent with the
lean-keybindings principle.

## Verification

1. `mise use -g "npm:@agentclientprotocol/claude-agent-acp"` succeeds and
   `which claude-agent-acp` resolves.
2. Restart Emacs (or `M-x load-file` the new module) with no errors; `agent-shell`,
   `acp`, and `shell-maker` are installed.
3. `C-c a` opens a Claude agent shell buffer.
4. A prompt sent in that buffer gets a response from Claude, authenticated via
   the existing login (no key prompt).

## Risks / open items

- The adapter binary name is `claude-agent-acp` (confirmed as the default of
  `agent-shell-anthropic-claude-acp-command`); the npm package
  `@agentclientprotocol/claude-agent-acp` is confirmed to exist on the registry.
- mise's `npm:` backend requires a node runtime available to mise. If the global
  install fails, fall back to a plain `npm install -g
  @agentclientprotocol/claude-agent-acp`.
