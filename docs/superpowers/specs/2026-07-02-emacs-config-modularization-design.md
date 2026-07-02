# Emacs config modularization & trim — design

**Date:** 2026-07-02
**Branch:** `bankruptcy`
**Status:** Decisions resolved — ready for implementation plan

## Resolved decisions

1. **Dropped old features are permanent cuts.** `denote`, old `org_mode` layer, and
   `janet`/`racket`/`web` are **not** migrated. (Recoverable from `master` if ever needed.)
2. **`doom-modeline` and the icon stack stay.**
3. **Module system:** `require` / `provide` (see rationale below).
4. **`bbww` dropped**, replaced by a self-written VSCode-style backward word delete.
5. **GC:** self-written mini-GCMH (high threshold during use + GC on idle timer).
6. **Org-mode:** new present-day comfort module (see Org section).

## Context

The refactor migrates a `straight.el` + `layers/` config into an Emacs Bedrock base.

- **Old (`master`):** 53-line `init.el` + ~17 files under `layers/`, loaded via a
  `taomacs-layers` list and `require`. Used `straight.el`, `lsp-mode`.
- **New (`bankruptcy`):** Bedrock base. Native `package.el` + `use-package`, `eglot`.
  But all config collapsed into one 787-line `init.el`, and Bedrock's stock `extras/*.el`
  are present but loaded by nothing (dead weight).

Scorecard: Goal 1 (trim/native) strong; Goal 2 (modularity) regressed to one file —
the primary work; Goal 3 (performance) good bones, one GC nit.

## Goals

1. Split the 787-line `init.el` into focused, independently-understandable modules.
2. Trim: remove experimental/dead code; prefer native and self-written elisp over small
   utility packages.
3. Preserve the performance setup; take the cheap steady-state GC win.

## Non-goals

- Re-adding denote / old-org / extra languages.
- Replacing substantial packages (magit, vertico/corfu/consult, avy, eglot).
- Byte-compiling modules (native-comp already covers loaded `.el`).

## Decision: module system — `require` / `provide`

Each module is a file on `load-path` ending in `(provide 'taomacs-<name>)`; `init.el`
loads them via a `taomacs-modules` list and `require`. Chosen over `load` because it gives
explicit cross-module dependency declaration and matches the convention the old config
used, at a cost of one `provide` line per file. Native-comp means no perf difference.

```elisp
;; init.el (target: ~30 lines)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(add-to-list 'load-path (locate-user-emacs-file "modules"))

(defvar taomacs-modules
  '(taomacs-core          ; defaults, backups, minibuffer, global keys, hooks
    taomacs-ui            ; theme, font, modeline, icons, tab-bar, hl-line
    taomacs-completion    ; vertico marginalia orderless corfu cape consult embark
    taomacs-editing       ; avy, self-written line/word motion+delete, yasnippet, autoinsert
    taomacs-dired         ; dired, dired-subtree, nerd-icons-dired
    taomacs-git           ; magit, diff-hl
    taomacs-shell         ; eshell, eat
    taomacs-dev           ; eglot, project, treesit, wgrep, which-key, exec-path, mise
    taomacs-org           ; org-mode comfort baseline
    taomacs-lang-lisp     ; clojure-ts-mode, cider, slime
    taomacs-lang-data))   ; yaml, json, markdown

(dolist (m taomacs-modules) (require m))

(let ((local (locate-user-emacs-file "local.el")))
  (when (file-exists-p local) (load local)))
```

## Target file structure

```
early-init.el          keep as-is (already good)
init.el                ~30-line loader (above)
custom.el              keep
local.el               keep (loaded last)
modules/
  taomacs-core.el
  taomacs-ui.el
  taomacs-completion.el
  taomacs-editing.el
  taomacs-dired.el
  taomacs-git.el
  taomacs-shell.el
  taomacs-dev.el
  taomacs-org.el
  taomacs-lang-lisp.el
  taomacs-lang-data.el
extras/                DELETE (unused Bedrock stock)
```

## Trim directives

1. **Remove SQL config** — the `sql` use-package block, the `sqls` eglot server
   registration, and the `sql-history` file. `lang-data` keeps yaml/json/markdown only.
2. **Remove all commented-out code** — dead `lsp-mode`, `clj-refactor`, `smartparens`,
   and the ~85-line `structural-editing` block, plus inline commented alternatives.
3. **Replace small utility packages:**

   | Package | Action | Notes |
   |---|---|---|
   | `crux` | **Delete** | Loaded but bound to nothing. |
   | `mwim` | **Self-write** | ~8-line smart `C-a`/`C-e`. |
   | `which-key` | **Drop `:ensure t`** | Built into Emacs 30.2. |
   | `bbww` | **Delete + self-write** | Replaced by VSCode-style backward delete (below). |

   Keepers: `avy`, `wgrep`, `diff-hl`, `magit`, vertico/corfu/consult stack,
   `exec-path-from-shell`, `mise`.

## Self-written editing helpers (`taomacs-editing.el`)

**VSCode-style backward word delete.** `backward-kill-word` saves to the kill-ring;
VSCode's word-delete does not. Replacement deletes without touching the kill-ring:

```elisp
(defun taomacs-backward-delete-word (arg)
  "Delete ARG words backward without saving to the kill ring (VSCode-style)."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
;; bound to M-DEL and C-<backspace>
```

**Smart line motion (replaces `mwim`).** `C-a` toggles between first non-whitespace and
column 0; `C-e` goes to end of line (~8 lines total, using `back-to-indentation` +
`beginning-of-line`).

## Org-mode comfort baseline (`taomacs-org.el`)

Present-day setup for someone getting comfortable with org — deliberately small, all native
(no org-modern etc.).

- **Locations:** `org-directory` = `~/org`; `org-default-notes-file` = `~/org/inbox.org`;
  `org-agenda-files` = the org directory. (Files created by org on first capture.)
- **Comfort visuals:** `org-startup-indented t` (org-indent), `org-hide-emphasis-markers t`,
  `org-pretty-entities t`, `org-ellipsis " ▾"`, `org-src-fontify-natively t`,
  `org-src-tab-acts-natively t`, `org-edit-src-content-indentation 0`,
  `org-return-follows-link t`, `org-fontify-quote-and-verse-blocks t`. (visual-line-mode
  and hl-line already inherited via existing text-mode hooks.)
- **Workflow:** `org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE"))`;
  `org-log-done 'time`.
- **Capture templates:** Todo and Note into `inbox.org`.
- **Keys — `C-c o` prefix** (avoids the existing `C-c a` = embark-act):
  `C-c o a` agenda, `C-c o c` capture, `C-c o l` store-link.

## Performance — mini-GCMH (`taomacs-core.el`)

Replace the end-of-init `gc-cons-threshold` restore-to-800000 with a small self-written
GC manager: keep a high threshold during activity and run `garbage-collect` on an idle
timer (and/or focus loss), so GC never pauses interaction. No `gcmh` package.

```elisp
(setq gc-cons-threshold (* 128 1024 1024))         ; generous during use
(run-with-idle-timer 5 t #'garbage-collect)        ; collect when idle
```

## Migration approach

Incremental, verifiable at every step ("loads clean" is the check — Emacs isn't unit-tested):

1. Create `modules/` + the thin `init.el` loader; move `core` out first.
2. Move one concern per commit into its module; after each, `emacs --debug-init` and confirm
   no errors + feature still works.
3. Apply trims (SQL, commented code, crux, which-key, mwim, bbww→self-written) as the owning
   module is created.
4. Add `taomacs-org.el`.
5. Delete `extras/` once unreferenced.
6. Apply the mini-GCMH change.

## Remaining micro-decisions (safe defaults chosen; adjust anytime)

- Org directory `~/org` and capture file `inbox.org` — change if you keep notes elsewhere.
- mini-GCMH threshold 128 MiB / 5 s idle — tunable.
