# Emacs Config Modularization Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Split the 787-line `init.el` into ~11 focused `require`/`provide` modules, trim dead/experimental code, replace small utility packages with native/self-written elisp, and add a comfortable org-mode module.

**Architecture:** `init.el` becomes a ~30-line loader: package setup + a `taomacs-modules` list `require`d in order from `modules/`. Each module owns one concern and ends with `(provide 'taomacs-<name>)`. The original `init.el` is preserved as `init.el.orig` (untracked) during migration so blocks can be copied verbatim, then deleted at the end.

**Tech Stack:** Emacs 30.2, `package.el` + `use-package`, native-comp.

## Global Constraints

- Emacs version floor: **30.2** (built-in `which-key`, `use-short-answers`, `setopt`).
- Package manager: built-in `package.el` + `use-package`. No `straight.el`.
- Prefer native / self-written elisp over small utility packages.
- Every module file ends with `(provide 'taomacs-<name>)` and starts with a
  `;;; taomacs-<name>.el --- <desc> -*- lexical-binding: t -*-` header line.
- Verification standard for every task: `emacs --debug-init` starts with **no** error
  and **no** `*Warnings*`/`*Backtrace*` popup, and the task's feature works. Automatable
  smoke form: `emacs --batch -l early-init.el -l init.el --eval '(message "LOADED OK")'`
  must print `LOADED OK` with no error (GUI-only features like fonts/doom-modeline are
  guarded by `display-graphic-p`, so batch load is representative of load-time errors).

---

## Module → content map (reference for all tasks)

| Module | Contains (blocks moved from `init.el.orig`) | New / changed |
|---|---|---|
| `taomacs-core` | the `emacs` use-package **minus appearance bits**; `midnight`; `ace-window`; `helpful` | mini-GCMH replaces gc restore; `taomacs-open-init-file`, `taomacs-backup-file-name` stay |
| `taomacs-ui` | theme load, font setup (`font-exists-p`), tab-bar + `display-time`, `doom-modeline` | — |
| `taomacs-completion` | `consult`, `embark`, `embark-consult`, `vertico`, `vertico-directory`, `marginalia`, `orderless`, `corfu`, `corfu-popupinfo`, `corfu-terminal`, `cape`, `kind-icon`, `nerd-icons-completion` | — |
| `taomacs-editing` | `avy`, `yasnippet`, `autoinsert` | self-written line motion (replaces `mwim`) + VSCode backward delete (replaces `bbww`) |
| `taomacs-dired` | `dired`, `dired-subtree`, `nerd-icons-dired` | — |
| `taomacs-git` | `magit`, `diff-hl` | — |
| `taomacs-shell` | `eshell`, `eat` | — |
| `taomacs-dev` | `eglot` (minus `sqls`), `project`, `wgrep`, `which-key`, `exec-path-from-shell`, `mise` | `which-key` drops `:ensure t` |
| `taomacs-org` | — | entire module is new |
| `taomacs-lang-lisp` | `clojure-ts-mode`, `cider`, `slime` | — |
| `taomacs-lang-data` | `markdown-mode`, `yaml-mode`, `json-mode` | — |

**Deleted entirely:** `sql` block, `crux`, `mwim`, `bbww`, the `sqls` eglot registration, the
`sql-history` file, and all commented-out code (`lsp-mode`, `clj-refactor`, `smartparens`,
`structural-editing`, inline commented alternatives). The `extras/` directory is deleted.

`treesit` remap/source-alist, `savehist`, `windmove`, `save-place`, `repeat-mode`,
`electric-pair`, resize keymap, world-clock, and all the `setopt` defaults live inside the
`emacs` use-package block → they travel with `taomacs-core`.

---

## Task 1: Scaffold loader + core module

**Files:**
- Preserve: `cp init.el init.el.orig` (untracked reference; add to `.gitignore` scope via not committing)
- Create: `modules/taomacs-core.el`
- Modify: `init.el` (replace entire contents with loader)

**Interfaces:**
- Produces: `taomacs-modules` (defvar), `modules/` on `load-path`, `(provide 'taomacs-core)`.

- [ ] **Step 1: Preserve original and create modules dir**

```bash
cp init.el init.el.orig
mkdir -p modules
```

- [ ] **Step 2: Write `modules/taomacs-core.el`**

Header + `(provide 'taomacs-core)` footer. Body = copy the entire `(use-package emacs …)`
form from `init.el.orig` **except** these appearance pieces (they move to `taomacs-ui` in
Task 3): the `load-theme` call, the `font-exists-p` defun and the `set-face-attribute`
font block, the `tab-bar-show`/`tab-bar-format`/`display-time-*`/`display-time-mode` lines.
Then append `midnight`, `ace-window`, and `helpful` use-package forms (copy verbatim from
`init.el.orig`). Finally add the mini-GCMH (replaces the old end-of-init gc restore):

```elisp
;;; taomacs-core.el --- Core editor defaults -*- lexical-binding: t -*-

;; ... (use-package emacs ...) with appearance bits removed, copied from init.el.orig ...
;; ... (use-package midnight ...) ...
;; ... (use-package ace-window ...) ...
;; ... (use-package helpful ...) ...

;; mini-GCMH: generous GC threshold during activity, collect when idle.
(setq gc-cons-threshold (* 128 1024 1024))
(run-with-idle-timer 5 t #'garbage-collect)

(provide 'taomacs-core)
;;; taomacs-core.el ends here
```

- [ ] **Step 3: Replace `init.el` with the loader**

```elisp
;;; init.el --- Module loader -*- lexical-binding: t -*-

;; Enable MELPA
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(add-to-list 'load-path (locate-user-emacs-file "modules"))

(defvar taomacs-modules
  '(taomacs-core)
  "Ordered list of config modules to load.")

(dolist (m taomacs-modules) (require m))

;; Load machine-local overrides last
(let ((local (locate-user-emacs-file "local.el")))
  (when (file-exists-p local) (load local)))
```

- [ ] **Step 4: Verify clean load**

Run: `emacs --debug-init` (GUI) — confirm no backtrace, core defaults active (e.g. `C-c e`
opens init, line numbers in prog-mode). Automatable: `emacs --batch -l early-init.el -l init.el --eval '(message "LOADED OK")'`
Expected: `LOADED OK`, no error.

- [ ] **Step 5: Commit**

```bash
git add init.el modules/taomacs-core.el
git commit -m "refactor: introduce module loader + core module"
```

---

## Task 2: UI module

**Files:** Create `modules/taomacs-ui.el`; Modify `init.el` (add `taomacs-ui` to list).

**Interfaces:** Consumes nothing from other modules. Produces `(provide 'taomacs-ui)`.

- [ ] **Step 1: Write `modules/taomacs-ui.el`**

Header + footer. Body = the appearance pieces removed in Task 1, plus `doom-modeline`:

```elisp
;;; taomacs-ui.el --- Appearance -*- lexical-binding: t -*-

(load-theme 'modus-operandi-tinted t)

(defun taomacs-font-exists-p (font)
  "Check if FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(when (taomacs-font-exists-p "IBM Plex Mono")
  (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))

;; tab-bar + clock (copied from init.el.orig)
(setopt tab-bar-show 1)
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T"
        display-time-interval 1)
(display-time-mode)

;; ... (use-package doom-modeline ...) copied from init.el.orig ...

(provide 'taomacs-ui)
;;; taomacs-ui.el ends here
```

Note: `font-exists-p` was renamed to `taomacs-font-exists-p` (namespaced; it was only used here).

- [ ] **Step 2: Add to loader** — edit `init.el` `taomacs-modules` to `'(taomacs-core taomacs-ui)`.

- [ ] **Step 3: Verify** — `emacs --debug-init`: theme + font + modeline + clock in tab-bar present, no error. Batch smoke form prints `LOADED OK`.

- [ ] **Step 4: Commit**

```bash
git add modules/taomacs-ui.el init.el
git commit -m "refactor: extract UI module"
```

---

## Task 3: Completion module

**Files:** Create `modules/taomacs-completion.el`; Modify `init.el`.

- [ ] **Step 1:** Write the module: header, then copy these blocks **verbatim** from
  `init.el.orig` in this order: `consult`, `embark-consult`, `embark`, `vertico`,
  `vertico-directory`, `marginalia`, `orderless`, `corfu`, `corfu-popupinfo`,
  `corfu-terminal`, `cape`, `kind-icon`, `nerd-icons-completion`. Footer `(provide 'taomacs-completion)`.
- [ ] **Step 2:** Add `taomacs-completion` to the loader list.
- [ ] **Step 3:** Verify — `emacs --debug-init`: `M-x`, `C-x b`, `C-s` (consult-line),
  `C-c a` (embark-act) all work; completion popup appears. Batch smoke prints `LOADED OK`.
- [ ] **Step 4:** Commit `refactor: extract completion module`.

---

## Task 4: Editing module (with self-written motion + VSCode delete)

**Files:** Create `modules/taomacs-editing.el`; Modify `init.el`.

**Interfaces:** Produces `taomacs-backward-delete-word`, `taomacs-beginning-of-line`,
`taomacs-end-of-line`, `(provide 'taomacs-editing)`.

- [ ] **Step 1: Write `modules/taomacs-editing.el`**

Copy `avy`, `yasnippet`, `autoinsert` verbatim from `init.el.orig`. Add the self-written
replacements for `mwim` and `bbww`:

```elisp
;;; taomacs-editing.el --- Editing helpers -*- lexical-binding: t -*-

;; --- Smart line motion (replaces the `mwim' package) ---
(defun taomacs-beginning-of-line ()
  "Move to first non-whitespace char, or to column 0 if already there."
  (interactive "^")
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point))
      (beginning-of-line))))

(defun taomacs-end-of-line ()
  "Move to end of line (kept for symmetry with `taomacs-beginning-of-line')."
  (interactive "^")
  (end-of-line))

(global-set-key (kbd "C-a") #'taomacs-beginning-of-line)
(global-set-key (kbd "C-e") #'taomacs-end-of-line)

;; --- VSCode-style backward word delete (replaces the `bbww' package) ---
;; Deletes the previous word WITHOUT saving it to the kill ring.
(defun taomacs-backward-delete-word (arg)
  "Delete ARG words backward without saving to the kill ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "M-DEL") #'taomacs-backward-delete-word)
(global-set-key (kbd "C-<backspace>") #'taomacs-backward-delete-word)

;; ... (use-package avy ...) copied from init.el.orig ...
;; ... (use-package yasnippet ...) copied from init.el.orig ...
;; ... (use-package autoinsert ...) copied from init.el.orig ...

(provide 'taomacs-editing)
;;; taomacs-editing.el ends here
```

- [ ] **Step 2:** Add `taomacs-editing` to the loader list.
- [ ] **Step 3: Verify motion + delete behavior**
  - `emacs --debug-init`. In a buffer with `    hello world`:
  - `C-a` → cursor at `hello` (first non-ws); `C-a` again → column 0. PASS.
  - Type `foo bar`, press `M-DEL` → deletes `bar`; `M-y` shows the *old* kill-ring entry,
    **not** `bar` (proves no kill-ring pollution). PASS.
  - `C-<backspace>` deletes previous word too. PASS.
- [ ] **Step 4:** Commit `refactor: editing module with self-written motion + delete`.

---

## Task 5: Dired module

**Files:** Create `modules/taomacs-dired.el`; Modify `init.el`.

- [ ] **Step 1:** Copy `dired`, `dired-subtree`, `nerd-icons-dired` verbatim; footer provide.
- [ ] **Step 2:** Add to loader list.
- [ ] **Step 3:** Verify — open a directory (`C-x d`), icons show, `TAB` toggles subtree, no error. Batch smoke `LOADED OK`.
- [ ] **Step 4:** Commit `refactor: extract dired module`.

---

## Task 6: Git module

**Files:** Create `modules/taomacs-git.el`; Modify `init.el`.

- [ ] **Step 1:** Copy `magit`, `diff-hl` verbatim; footer provide.
- [ ] **Step 2:** Add to loader list.
- [ ] **Step 3:** Verify — `C-x g` opens magit; diff-hl gutter marks show in a modified file. Batch smoke `LOADED OK`.
- [ ] **Step 4:** Commit `refactor: extract git module`.

---

## Task 7: Shell module

**Files:** Create `modules/taomacs-shell.el`; Modify `init.el`.

- [ ] **Step 1:** Copy `eshell`, `eat` verbatim; footer provide.
- [ ] **Step 2:** Add to loader list.
- [ ] **Step 3:** Verify — `C-c t` toggles eshell; `less`/`git log` render via eat. Batch smoke `LOADED OK`.
- [ ] **Step 4:** Commit `refactor: extract shell module`.

---

## Task 8: Dev module (drops `which-key :ensure`, drops `sqls`)

**Files:** Create `modules/taomacs-dev.el`; Modify `init.el`.

- [ ] **Step 1:** Write module. Copy `project`, `wgrep`, `exec-path-from-shell`, `mise`
  verbatim. Copy `eglot` **but remove** the `(add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))`
  line. Copy `which-key` **but remove `:ensure t`** (built-in on 30.2):

```elisp
;; which-key is built into Emacs 30.2 — no :ensure needed
(use-package which-key
  :config
  (which-key-mode))
```

  Footer `(provide 'taomacs-dev)`.
- [ ] **Step 2:** Add to loader list.
- [ ] **Step 3:** Verify — `emacs --debug-init`: `C-x` shows which-key popup; `M-x eglot`
  in a project starts a server; `project-mode-line` shows project name. Batch smoke `LOADED OK`.
- [ ] **Step 4:** Commit `refactor: extract dev module; drop which-key ensure and sqls`.

---

## Task 9: Org module (new)

**Files:** Create `modules/taomacs-org.el`; Modify `init.el`.

**Interfaces:** Produces `(provide 'taomacs-org)`. Binds under `C-c o` (no conflict with
`C-c a` = embark-act).

- [ ] **Step 1: Write `modules/taomacs-org.el`**

```elisp
;;; taomacs-org.el --- Comfortable org-mode baseline -*- lexical-binding: t -*-

(use-package org
  ;; built-in — no :ensure
  :init
  (setopt org-directory "~/org"
          org-default-notes-file (expand-file-name "inbox.org" "~/org")
          org-agenda-files (list "~/org"))
  :custom
  ;; Comfort visuals
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-return-follows-link t)
  (org-fontify-quote-and-verse-blocks t)
  ;; Workflow
  (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
  (org-log-done 'time)
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %U")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\n  %U")))
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o l" . org-store-link)))

(provide 'taomacs-org)
;;; taomacs-org.el ends here
```

- [ ] **Step 2:** Add `taomacs-org` to the loader list.
- [ ] **Step 3: Verify**
  - `emacs --debug-init`, then `C-c o c` → capture menu shows Todo/Note; capturing a Todo
    creates `~/org/inbox.org` with a `* TODO` under `Tasks`. PASS.
  - Open `~/org/inbox.org`: headings are indented (org-indent), `*bold*` markers hidden. PASS.
  - `C-c o a` opens the agenda over `~/org`. PASS.
  - Batch smoke `LOADED OK`.
- [ ] **Step 4:** Commit `feat: add comfortable org-mode module`.

---

## Task 10: Language modules

**Files:** Create `modules/taomacs-lang-lisp.el` and `modules/taomacs-lang-data.el`; Modify `init.el`.

- [ ] **Step 1:** `taomacs-lang-lisp.el` — copy `clojure-ts-mode`, `cider`, `slime` verbatim
  (skip the commented `clj-refactor`, `smartparens`, `structural-editing`); footer provide.
- [ ] **Step 2:** `taomacs-lang-data.el` — copy `markdown-mode`, `yaml-mode`, `json-mode`
  verbatim (**not** `sql` — deleted); footer provide.
- [ ] **Step 3:** Add both to the loader list (order: `taomacs-lang-lisp taomacs-lang-data`).
- [ ] **Step 4:** Verify — open a `.clj` (clojure-ts-mode), `.json`, `.yaml`, `.md` file; correct
  major modes activate; `M-x slime` starts if `sbcl` present. Batch smoke `LOADED OK`.
- [ ] **Step 5:** Commit `refactor: extract language modules; drop sql`.

---

## Task 11: Cleanup — delete leftovers

**Files:** Delete `init.el.orig`, `extras/`, `sql-history`.

- [ ] **Step 1: Confirm nothing references the deletions**

Run: `grep -rn "extras\|sql-history\|init.el.orig" init.el modules/ early-init.el`
Expected: no matches.

- [ ] **Step 2: Delete**

```bash
rm -rf extras/ init.el.orig sql-history
```

- [ ] **Step 3: Final full verification**

Run: `emacs --debug-init` — full config loads clean; spot-check one feature per module.
Automatable: `emacs --batch -l early-init.el -l init.el --eval '(message "LOADED OK")'` → `LOADED OK`.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "chore: remove unused extras/, sql-history, and migration backup"
```

---

## Self-review notes

- **Spec coverage:** module system (Task 1), all 11 modules (Tasks 1–3,5–10), SQL removal
  (Tasks 8,10), commented-code removal (Tasks 1,10), crux delete (dropped by omission from
  every module — verify with grep in Task 11), mwim/bbww self-write (Task 4), which-key
  native (Task 8), mini-GCMH (Task 1), org (Task 9), extras delete (Task 11). ✓
- **`crux` note:** it is deleted simply by never copying it into any module; the Task 11
  grep `grep -rn "crux" modules/` should return nothing — add that to Step 1 if desired.
- **Namespacing:** `font-exists-p` → `taomacs-font-exists-p` (Task 2) since it was global
  and only used for the font check.
```
