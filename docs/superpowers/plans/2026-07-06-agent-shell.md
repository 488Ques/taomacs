# agent-shell for Claude Code Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a native Emacs `agent-shell` for Claude, launched with `C-c a`, reusing the existing Claude Code login.

**Architecture:** A new `use-package`-based config module `modules/taomacs-ai.el` installs `agent-shell` from MELPA (which pulls `acp.el` + `shell-maker`), configures login-based Anthropic auth, and binds the Claude launcher. The module is registered in `init.el`'s `taomacs-modules` list. An external `claude-agent-acp` adapter, installed via mise, provides the agent process on `PATH`.

**Tech Stack:** Emacs 30, `use-package`, MELPA, `agent-shell` / `acp.el` / `shell-maker`, mise (npm backend), `exec-path-from-shell`.

## Global Constraints

- Config modules are named `taomacs-*.el`, live in `modules/`, end with `(provide 'taomacs-NAME)`, and are registered in the `taomacs-modules` list in `init.el`.
- Packages are installed with `use-package` + `:ensure t` from MELPA.
- Indentation uses tabs, matching existing modules.
- Git commits: no `Co-Authored-By` / AI-attribution trailer.
- Keep keybindings lean: bind only the single frequently-used launcher; leave other commands to `M-x`.

---

### Task 0 (prerequisite — user runs this, not the implementer): Install the ACP adapter

This is an external, one-time step performed by the user in a terminal. The
implementer does not run it but the final verification depends on it.

```bash
mise use -g "npm:@agentclientprotocol/claude-agent-acp"
which claude-agent-acp   # should print a path
```

If mise's npm backend fails, fall back to:

```bash
npm install -g @agentclientprotocol/claude-agent-acp
```

Authentication is inherited from the existing Claude Code login; no key is stored.

---

### Task 1: Add the `taomacs-ai` module and register it

**Files:**
- Create: `modules/taomacs-ai.el`
- Modify: `init.el` (add `taomacs-ai` to the `taomacs-modules` list, after `taomacs-shell`)

**Interfaces:**
- Consumes: `agent-shell` package from MELPA, which provides
  `agent-shell-anthropic-make-authentication` (keyword args `:login` /
  `:api-key` / `:oauth`), the custom variable `agent-shell-anthropic-authentication`,
  and the interactive command `agent-shell-anthropic-start-claude-code`.
- Produces: feature `taomacs-ai` (via `provide`); global binding `C-c a`.

- [ ] **Step 1: Create `modules/taomacs-ai.el`**

Use tabs for indentation to match the other modules.

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

- [ ] **Step 2: Register the module in `init.el`**

In the `taomacs-modules` list, add `taomacs-ai` on its own line immediately
after `taomacs-shell`:

```elisp
    taomacs-shell
    taomacs-ai
    taomacs-dev
```

- [ ] **Step 3: Verify the module byte-compiles cleanly**

Run from the Emacs config root:

```bash
emacs --batch -L modules -f package-initialize \
  --eval '(byte-compile-file "modules/taomacs-ai.el")'
```

Expected: no errors. Warnings about `agent-shell` functions being undefined at
compile time are acceptable only if the package is not yet installed; the load
test in Step 4 is the real gate. Delete any generated `.elc`:

```bash
rm -f modules/taomacs-ai.elc
```

- [ ] **Step 4: Verify a full Emacs startup loads the module and binds the key**

```bash
emacs --batch -l early-init.el -l init.el \
  --eval '(progn
            (unless (featurep (quote taomacs-ai)) (error "taomacs-ai not loaded"))
            (unless (keymapp (current-global-map)) (error "no global map"))
            (let ((cmd (keymap-lookup (current-global-map) "C-c a")))
              (unless (eq cmd (quote agent-shell-anthropic-start-claude-code))
                (error "C-c a not bound to Claude launcher, got %S" cmd)))
            (princ "OK: taomacs-ai loaded and C-c a bound\n"))'
```

Expected output ends with: `OK: taomacs-ai loaded and C-c a bound`

This confirms `agent-shell` installed from MELPA (pulling `acp.el` and
`shell-maker`), the module loaded, and the binding is live.

- [ ] **Step 5: Commit**

```bash
git add modules/taomacs-ai.el init.el
git commit -m "feat: add agent-shell module for Claude Code"
```

---

### Task 2 (manual, interactive — cannot be batch-tested): End-to-end smoke test

Batch mode cannot exercise the interactive agent shell, so this is a manual
check the user performs after Task 1.

- [ ] **Step 1:** Confirm `which claude-agent-acp` resolves in the same shell environment Emacs inherits (Task 0 done).
- [ ] **Step 2:** Start Emacs normally (GUI) and press `C-c a`.
- [ ] **Step 3:** Confirm a Claude agent shell buffer opens with no authentication prompt (login inherited).
- [ ] **Step 4:** Send a short prompt (e.g. "say hello") and confirm Claude responds.

If step 3 prompts for auth or step 4 errors on the adapter, re-check Task 0
(`claude-agent-acp` on `PATH`) and that `exec-path-from-shell` has propagated the
mise shims into Emacs (`M-: (executable-find "claude-agent-acp")`).

---

## Self-Review

- **Spec coverage:** Module creation, init.el registration, login auth, `C-c a`
  binding, mise adapter install, and verification steps all map to tasks above.
  ✓
- **Placeholder scan:** No TBD/TODO; all code and commands are concrete. ✓
- **Type consistency:** `agent-shell-anthropic-make-authentication`,
  `agent-shell-anthropic-authentication`, and
  `agent-shell-anthropic-start-claude-code` are used identically in the spec,
  module, and verification. ✓
