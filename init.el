;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Use the official package archives.
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(use-package taomacs-bootstrap
  :load-path "lisp"
  :demand t
  :config
  (taomacs-bootstrap-refresh-package-archives))

;;; External tool environments

;; Refresh the base environment on GUI/daemon startup, independent of Emacs Plus.
(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x pgtk)) (daemonp))
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; Apply project-specific tools and variables after the base environment is ready.
(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode))

;;; Editor defaults

(use-package taomacs-files
  :load-path "lisp"
  :demand t)

;; Core configuration
(use-package emacs
  :config

  ;; Word wrap
  (global-visual-line-mode 1)

  ;; Enable imenu to include 'use-package' declarations
  (setopt use-package-enable-imenu-support t)

  (setopt
   ;; Turn off the welcome screen
   inhibit-splash-screen t
   ;; this information is useless for most
   display-time-default-load-average nil
   ;; Fix archaic defaults
   sentence-end-double-space nil
   ;; Show columns alongside the default line-number display
   column-number-mode t
   ;; Make switching buffers more consistent
   switch-to-buffer-obey-display-actions t
   ;; Show buffer top and bottom in the margin
   indicate-buffer-boundaries 'left
   ;; Enable horizontal scrolling
   mouse-wheel-tilt-scroll t
   mouse-wheel-flip-direction t
   ;; Set a minimum width for line numbers
   display-line-numbers-width 3
   ;; Ask before quitting Emacs, even when there are no unsaved buffers
   confirm-kill-emacs #'y-or-n-p
   ;; y/n instead of yes/no when prompted
   use-short-answers t)

  (setopt
   auto-revert-avoid-polling t
   auto-revert-check-vc-info t)

  ;; Don't litter file system with *~ backup files; put them all inside
  ;; ~/.emacs.d/backup or wherever
  (setopt make-backup-file-name-function 'taomacs-backup-file-name)

  ;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
  (setopt
   ;; Use the minibuffer whilst in the minibuffer
   enable-recursive-minibuffers t
   ;; TAB cycles candidates
   completion-cycle-threshold 1
   ;; Show annotations
   completions-detailed t
   ;; When I hit TAB, try to complete, otherwise, indent
   tab-always-indent 'complete)

  (setopt
   ;; Open completion always; `lazy' another option
   completion-auto-help 'always
   ;; This is arbitrary
   completions-max-height 20
   ;; Display the completions down the screen in one column
   completions-format 'one-column
   ;; Enable grouping of completion candidates
   completions-group t
   ;; Much more eager
   completion-auto-select 'second-tab)

  ;; Banish the Custom stuff
  (setopt custom-file (locate-user-emacs-file "custom.el"))

  ;; Automatically reread from disk if the underlying file changes
  (global-auto-revert-mode)

  ;; Save history of minibuffer
  (savehist-mode)

  ;; Move through windows with Ctrl-<arrow keys>
  (windmove-default-keybindings 'control) ; You can use other modifiers here

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  ;; Steady cursor
  (blink-cursor-mode -1)

  ;; For terminal users, make the mouse more useful
  (xterm-mouse-mode 1)

  ;; Auto parenthesis matching
  (electric-pair-mode 1)

  ;; Remember and restore the last cursor location of opened files
  (save-place-mode 1)

  ;; Unset the annoying zooming in behavior when pinching on a touchpad
  (global-unset-key (kbd "<pinch>"))

  ;; Disable accidental text zoom when holding Control while scrolling
  (global-unset-key (kbd "<C-wheel-up>"))
  (global-unset-key (kbd "<C-wheel-down>"))
  (global-unset-key (kbd "<C-mouse-4>"))
  (global-unset-key (kbd "<C-mouse-5>"))

  ;; Unset this key since I have a habit of pressing it for undo
  (global-unset-key (kbd "C-z"))

  ;; tab-bar
  (setopt tab-bar-show 1)
  ;; s-1 .. s-9 jump straight to a tab by number
  (setopt tab-bar-select-tab-modifiers '(super))

  ;; Repeat keybinding
  (repeat-mode 1)

  (defvar-keymap taomacs-resize-window-keymap
    :repeat t
    "h" #'shrink-window-horizontally
    "l" #'enlarge-window-horizontally
    "j" #'shrink-window
    "k" #'enlarge-window)
  (keymap-global-set "C-c r" taomacs-resize-window-keymap)

  ;; Setup list of times to track
  (setopt world-clock-list '(("Asia/Ho_Chi_Minh" "Vietnam")
			     ("Poland" "Poland")
			     ("Portugal" "Portugal")
			     ("America/New_York" "US EST")))

  :hook
  ;; Display line numbers in programming mode
  (prog-mode . display-line-numbers-mode)
  ;; Modes to highlight the current line with
  ((text-mode prog-mode) . hl-line-mode)
  ;; Clean up whitespace
  (before-save . whitespace-cleanup)

  :bind
  (("C-c e" . taomacs-open-init-file)
   ("C-/" . undo-only)
   ("C-c u" . undo-redo)

   ("C-;" . comment-line)

   ("M-n" . scroll-up-line)
   ("M-p" . scroll-down-line)

   :map minibuffer-mode-map
   ("TAB" . minibuffer-complete)))

;; Built-in pixel scrolling for trackpads and high-resolution mice.
(use-package pixel-scroll
  :config
  (pixel-scroll-precision-mode 1))

;; Run actions at midnight
;; By defaul clean up unused buffers at midnight
(use-package midnight
  :hook (after-init . midnight-mode))

;; Enhancement for window navigation
(use-package taomacs-window
  :load-path "lisp"
  :demand t)

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
	 ("s-]" . other-window)
	 ("s-[" . taomacs-previous-window)))

;; mini-GCMH: generous GC threshold during activity, collect when idle.
(setq gc-cons-threshold (* 128 1024 1024))
(run-with-idle-timer 5 t #'garbage-collect)

;;; Appearance

(load-theme 'modus-operandi-tinted t)

(use-package taomacs-fonts
  :load-path "lisp"
  :demand t)

(when (taomacs-font-exists-p "Iosevka")
  (set-face-attribute 'default nil :family "Iosevka" :height 150)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 150))

(when (taomacs-font-exists-p "Iosevka Etoile")
  (set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 150))

;; Keep private-use glyph fallback for plain terminal output (e.g. an
;; oh-my-pi prompt inside Ghostel), independently of Emacs UI icons.
(when (taomacs-font-exists-p "Symbols Nerd Font Mono")
  (dolist (range '((#xe000 . #xf8ff)      ; BMP private use: most Nerd Font icons
		   (#xf0000 . #xffffd)))  ; plane 15: Material Design Icons
    (set-fontset-font t range "Symbols Nerd Font Mono" nil 'prepend)))

;; Disable clock
(display-time-mode -1)

;; Keep the useful context without encoding, scroll, or minor-mode clutter.
(use-package doom-modeline
  :ensure t
  :demand t
  :custom
  (doom-modeline-height 24)
  (doom-modeline-icon (taomacs-font-exists-p "Symbols Nerd Font Mono"))
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-env-version nil)
  (doom-modeline-position-column-line-format '("L%l:C%c"))
  :config
  (doom-modeline-mode 1))

;;; Search and completion

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
	 ;; Drop-in replacements
	 ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
	 ("M-y"   . consult-yank-pop)   ; orig. yank-pop
	 ("M-i" . consult-imenu)      ; orig. imenu
	 ;; Diagnostics (eglot feeds flymake); C-u for whole-project errors
	 ("M-g f" . consult-flymake)
	 ;; Searching
	 ("M-s r" . consult-ripgrep)
	 ("C-s" . consult-line)
	 ("M-s s" . consult-line)
	 ("M-s L" . consult-line-multi)
	 ("M-s o" . consult-outline)
	 ;; Isearch integration
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
	 ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
	 )
  :config
  ;; Prefix for narrowing completion candidates by source.
  (setq consult-narrow-key "<"))

;; Integration between embark and consult
(use-package embark-consult
  :ensure t
  :after (embark consult))

;; Context-sensitive actions, also available through Avy's "." dispatch.
(use-package embark
  :ensure t
  :bind ("C-c a" . embark-act))

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  :bind
  (:map vertico-map
	;; Improve directory navigation
	("DEL" . vertico-directory-delete-char)))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	      ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :defer t
  :init
  (setq completion-styles '(orderless)))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("C-t" . corfu-insert-separator)
	("C-n" . corfu-next)
	("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil))

;; Filename completion, autoloaded when requested.
(use-package cape
  :ensure t
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; Editing and navigation

(use-package taomacs-editing
  :load-path "lisp"
  :demand t)

(global-set-key (kbd "C-a") #'taomacs-beginning-of-line)
(global-set-key (kbd "C-e") #'end-of-line)

(global-set-key (kbd "M-DEL") #'taomacs-backward-delete-word)
(global-set-key (kbd "C-<backspace>") #'taomacs-backward-delete-line)

(use-package taomacs-avy
  :load-path "lisp"
  :demand t)

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-line)
	 ("s-j" . avy-goto-char-timer))
  :config
  (setf (alist-get ?. avy-dispatch-alist) #'taomacs-avy-action-embark))

;;; Help

;; which-key is built into Emacs 30.2 — no :ensure needed
(use-package which-key
  :config
  (which-key-mode))

;; Better interface for Emacs' help
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

;;; Files and Dired

;; Dired: file manager
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t)                  ;; do what I mean
  (setq dired-recursive-copies 'always)       ;; don't ask when copying directories
  (setq dired-create-destination-dirs 'ask)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-mouse-drag-files t)
  (setq dired-kill-when-opening-new-dired-buffer t)   ;; Tidy up open buffers by default
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq dired-use-ls-dired t
	      insert-directory-program gls
	      dired-listing-switches "-aBhl  --group-directories-first")))))

;;; Version control

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :config
  ;; 1. Replace the "OR" function with the specific "Unpushed" function
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-unpushed-to-upstream
			  'magit-insert-unpushed-to-upstream-or-recent
			  'replace)

  ;; 2. Add the "Recent" function explicitly after the "Unpushed" one
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-recent-commits
			  'magit-insert-unpushed-to-upstream
			  t)

  (setopt magit-log-section-commit-count 20)

  :bind (("C-x g" . magit-status)))

;; Indication of local VCS changes
(use-package diff-hl
  :ensure t
  :hook
  ;; Enable `diff-hl' support by default in programming buffers
  (prog-mode . diff-hl-mode)
  :config
  ;; Update the highlighting without saving
  (diff-hl-flydiff-mode t))

;;; Shells and terminals

;; Bind after the library that owns Eshell's keymap has loaded.
(use-package esh-mode
  :bind (:map eshell-mode-map
              ("C-r" . consult-history)))

;; Ghostel: terminal emulator powered by libghostty (the Ghostty VT engine).
;; The native module lives in the package dir; `M-x ghostel-download-module'
;; re-fetches it after an upgrade.
(use-package ghostel
  :ensure t

  ;; :init, not :config -- this has to run before ghostel is first loaded,
  ;; otherwise "Ghostel" is missing from the C-x p p menu until then.
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))

  ;; Full-screen programs need a terminal emulator, not Eshell's output buffer.
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "omp"))

  :hook
  (eshell-load . ghostel-eshell-visual-command-mode)

  :bind
  (("C-c t" . ghostel)                 ; C-u C-c t: another terminal
   :map project-prefix-map
   ("t" . ghostel-project)))           ; C-x p t: terminal in the current project

(use-package taomacs-ghostel
  :load-path "lisp"
  :after ghostel
  :demand t
  :bind (:map ghostel-readonly-mode-map
	      ("M-w" . taomacs-ghostel-copy-stay)
	      ("C-w" . taomacs-ghostel-copy-stay)))

;;; Sessions

;; Named desktop.el sessions, one active at a time: buffers, frames, tabs,
;; window layouts, and Ghostel/Eshell directories.  Commands live in
;; lisp/taomacs-session.el; sessions are saved under sessions/NAME/.
(use-package desktop
  :defer t
  :custom
  ;; Save the active session without asking.  With no active session
  ;; `desktop-save-mode' is off, so nothing is saved.
  (desktop-save t)
  ;; Histories, registers, and the kill ring stay global across sessions;
  ;; savehist persists the minibuffer histories.
  (desktop-globals-to-save '(desktop-missing-file-warning
                             tags-file-name tags-table-list))
  (desktop-globals-to-clear nil))

(use-package taomacs-session
  :load-path "lisp"
  :hook ((eshell-mode . taomacs-session-setup-eshell)
         (magit-status-mode . taomacs-session-setup-magit))
  :bind (("C-c s s" . taomacs-session-save)     ; C-u: save under another name
         ("C-c s r" . taomacs-session-switch)
         ("C-c s v" . taomacs-session-revert)
         ("C-c s c" . taomacs-session-close)
         ("C-c s k" . taomacs-session-delete)))

;; Start another Emacs instance, e.g. to work in a second session at once.
(use-package taomacs-instance
  :load-path "lisp"
  :bind ("C-c n" . taomacs-instance-start))

;;; Development tools

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;; Project management
(use-package project
  :config
  (setopt project-vc-extra-root-markers '("deps.edn")))

;; Emacs 31 manages mode routing and prompts to build missing grammars.
(use-package treesit
  :config
  (setopt treesit-enabled-modes
          '(bash-ts-mode css-ts-mode js-ts-mode json-ts-mode python-ts-mode
            typescript-ts-mode tsx-ts-mode yaml-ts-mode go-ts-mode go-mod-ts-mode)
          treesit-auto-install-grammar 'ask))

(use-package taomacs-eldoc
  :load-path "lisp"
  :demand t)

;; Helpful resources:
;; - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
(use-package eglot
  ;; Emacs 31 includes pull diagnostics, including TypeScript 7 support.
  :defer t
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
  ;; Disable event logging through Eglot's public option, not a JSONRPC override.
  (eglot-events-buffer-config '(:size 0 :format full)))

;; Async, cursor-preserving formatting with Apheleia's global defaults.
(use-package apheleia
  :ensure t
  :config
  ;; Preserve the real filename for nested oxfmt configs and file overrides.
  (setf (alist-get 'oxfmt apheleia-formatters)
        '("apheleia-npx" "oxfmt" "--stdin-filepath" filepath)
        (alist-get 'typescript-ts-mode apheleia-mode-alist) 'oxfmt
        (alist-get 'tsx-ts-mode apheleia-mode-alist) 'oxfmt)
  (apheleia-global-mode 1))

;;; Org

(use-package org
  ;; built-in — no :ensure
  :init
  (setopt org-directory "~/org"
	  org-default-notes-file (expand-file-name "inbox.org" "~/org")
	  org-agenda-files (list "~/org/inbox.org"))
  :custom
  ;; Comfort visuals
  (org-startup-indented t)
  (org-ellipsis " ▾")
  (org-edit-src-content-indentation 0)
  (org-return-follows-link t)
  (org-fontify-quote-and-verse-blocks t)
  ;; Workflow
  (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "ARCHIVED")))
  (org-log-done 'time)
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\nCreated at: %U")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\nCreated at: %U")))
  :hook
  (org-mode . variable-pitch-mode)
  ;; Keep manual completion available without automatic popups in Org buffers.
  (org-mode . (lambda () (setq-local corfu-auto nil)))
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o l" . org-store-link))
  :config
  ;; Keep aligned content monospaced when prose uses variable-pitch.
  (dolist (face '(org-code org-verbatim org-block org-table org-formula
			   org-checkbox org-meta-line org-block-begin-line
			   org-block-end-line))
    (set-face-attribute face nil :inherit 'fixed-pitch))
  ;; Org's virtual indentation must remain hidden as well as monospaced.
  (with-eval-after-load 'org-indent
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))

;;; Languages

;;;; Lisp

;; Clojure major mode that uses Tree-sitter
(use-package clojure-ts-mode
  :ensure t)

;; Clojure REPL
(use-package cider
  :ensure t)

;; Emacs support for Common Lisp
(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")

  :hook
  ((slime-mode . (lambda () (setq-local corfu-popupinfo-delay nil)))
   (slime-repl-mode . (lambda () (setq-local corfu-popupinfo-delay nil))))

  :config
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl)))

;;;; Data and markup

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package js
  :defer t
  :mode (("\\.jsonld\\'" . js-json-mode)
         ("\\(?:\\.babelrc\\|\\.bowerrc\\|composer\\.lock\\)\\'" . js-json-mode))
  :magic-fallback ("^[{[]$" . js-json-mode))

;;;; Go

(use-package taomacs-go
  :load-path "lisp"
  :demand t)

;; Auto-start Eglot for Go; its built-in mapping uses gopls.  Eglot
;; selects the project root via project.el, while gopls understands go.mod.
(use-package go-ts-mode
  ;; built-in (Emacs 30)---no :ensure
  :hook ((go-ts-mode . eglot-ensure)
	 (go-ts-mode . taomacs-go-ts-setup))
  :custom
  (go-ts-mode-indent-offset 4))

;;;; Web

(use-package taomacs-typescript
  :load-path "lisp"
  :demand t)

;; Auto-start Eglot for TS/TSX with the version-aware server contact above.
;; Native Tree-sitter management handles .ts/.tsx mode routing.
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook        #'eglot-ensure)

;; Astro (light): clean highlighting of the frontmatter + HTML + JSX mix.
;; No language server; formatting follows Apheleia's web-mode default.
(use-package web-mode
  :ensure t
  :mode "\\.astro\\'")

;;; Databases

;; Configure named connections with `sql-connection-alist' in local.el.
;; Keep passwords in auth-source, not in the connection list.
;; Built-in `sql.el' -- REPL, send-region, object listing.  No :ensure.
(use-package sql
  :bind (("C-c d c" . sql-connect)             ; connect to a named database
	 ("C-c d b" . sql-show-sqli-buffer)))   ; show/switch the SQLi REPL

;; Load machine-local overrides last
(let ((local (locate-user-emacs-file "local.el")))
  (when (file-exists-p local) (load local)))
