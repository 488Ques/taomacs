;; Enable MELPA packages
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Core configuration
(use-package emacs
  :config

  ;; This function creates nested directories in the backup folder. If
  ;; instead you would like all backup files in a flat structure, albeit
  ;; with their full paths concatenated into a filename, then you can
  ;; use the following configuration:
  ;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
  ;;
  ;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
  ;;   (setopt backup-directory-alist `(("." . ,backup-dir))))
  (defun taomacs-backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
	   (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
	   (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))

  ;; Function to quickly open the init.el file
  (defun taomacs-open-init-file ()
    "Open init.el file for editing."
    (interactive)
    (find-file user-init-file))

  ;; Check whether a given font exists
  (defun font-exists-p (font)
    "Check if the FONT exists."
    (and (display-graphic-p) (not (null (x-list-fonts font)))))

  ;; Use the excellent modus-operandi-tinted theme
  (load-theme 'modus-operandi-tinted t)

  (when (font-exists-p "IBM Plex Mono")
    (set-face-attribute 'default nil
			:family "IBM Plex Mono"
			:height 140))

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
   ;; Show current line in modeline
   line-number-mode t
   ;; Show column as well
   column-number-mode t
   ;; Prettier underlines
   x-underline-at-descent-line nil
   ;; Make switching buffers more consistent
   switch-to-buffer-obey-display-actions t
   ;; Prettier underlines
   x-underline-at-descent-line nil
   ;; Make switching buffers more consistent
   switch-to-buffer-obey-display-actions t
   ;; By default, don't underline trailing spaces
   show-trailing-whitespace nil
   ;; Show buffer top and bottom in the margin
   indicate-buffer-boundaries 'left
   ;; Enable horizontal scrolling
   mouse-wheel-tilt-scroll t
   mouse-wheel-flip-direction t
   ;; Set a minimum width for line numbers
   display-line-numbers-width 3
   ;; y/n instead of yes/no when prompted
   use-short-answers t)

  ;; We won't set these, but they're good to know about
  ;; (setopt indent-tabs-mode nil)
  ;; (setopt tab-width 4)

  (setopt
   auto-revert-avoid-polling t
   ;; Some systems don't do file notifications well; see
   ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
   auto-revert-interval 5
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
   ;; See `C-h v completion-auto-select' for more possible values
   ;; completion-auto-select t
   ;; Different styles to match input to candidates
   ;; completion-styles '(basic initials substring)
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

  ;; Tell Emacs to prefer the treesit mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
	'((yaml-mode . yaml-ts-mode)
	  (bash-mode . bash-ts-mode)
	  (js2-mode . js-ts-mode)
	  (typescript-mode . typescript-ts-mode)
	  (json-mode . json-ts-mode)
	  (css-mode . css-ts-mode)
	  (python-mode . python-ts-mode)))

  ;; Set sources for treesit language grammar
  (setq treesit-language-source-alist
	'((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))))

  ;; Show the tab-bar as soon as tab-bar functions are invoked
  (setopt tab-bar-show 1)
  ;; Add the time to the tab-bar, if visible
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
  (setopt display-time-format "%a %F %T")
  (setopt display-time-interval 1)
  (display-time-mode)

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
  ;; Smooth scrolling
  (pixel-scroll-precision-mode)

  ;; Use common keystrokes by default
  ;; (cua-mode)

  ;; For terminal users, make the mouse more useful
  (xterm-mouse-mode 1)

  ;; Auto parenthesis matching
  (electric-pair-mode 1)

  ;; Remember and restore the last cursor location of opened files
  (save-place-mode 1)

  ;; Unset the annoying zooming in behavior when pinching on a touchpad
  (global-unset-key (kbd "<pinch>"))

  ;; Unset this key since I have a habit of pressing it for undo
  (global-unset-key (kbd "C-z"))

  ;; Repeat keybinding
  (repeat-mode 1)

  (defvar-keymap taomacs-resize-window-keymap
    :repeat t
    "h" #'shrink-window-horizontally
    "l" #'enlarge-window-horizontally
    "j" #'shrink-window
    "k" #'enlarge-window)

  ;; Setup list of times to track
  (setopt world-clock-list '(("Asia/Ho_Chi_Minh" "Vietnam")
			     ("Poland" "Poland")
			     ("Portugal" "Portugal")
			     ("America/New_York" "US EST")))

  :hook
  ;; Display line numbers in programming mode
  (prog-mode . display-line-numbers-mode)
  ;; Nice line wrapping when working with text
  (text-mode . visual-line-mode)
  ;; Modes to highlight the current line with
  ((text-mode prog-mode) . hl-line-mode)
  ;; Clean up whitespace
  (before-save . whitespace-cleanup)

  :bind
  (("C-c e" . taomacs-open-init-file)
   ("C-;" . comment-line)

   :map minibuffer-mode-map
   ("TAB" . minibuffer-complete))

  :bind-keymap
  ("C-c r" . taomacs-resize-window-keymap))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :config
  (which-key-mode))

;; Run actions at midnight
;; By defaul clean up unused buffers at midnight
(use-package midnight
  :hook (after-init . midnight-mode))

;; Dired: file manager
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t)                  ;; do what I mean
  (setq dired-recursive-copies 'always)       ;; don't ask when copying directories
  (setq dired-create-destination-dirs 'ask)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-make-directory-clickable t)
  (setq dired-mouse-drag-files t)
  (setq dired-kill-when-opening-new-dired-buffer t)   ;; Tidy up open buffers by default
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq dired-use-ls-dired t
	      insert-directory-program gls
	      dired-listing-switches "-aBhl  --group-directories-first")))))

;; Toggle a directory to show items inside it
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
	      ("TAB" . dired-subtree-toggle)))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
	 ;; Drop-in replacements
	 ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
	 ("M-y"   . consult-yank-pop)   ; orig. yank-pop
	 ("M-i" . consult-imenu)      ; orig. imenu
	 ;; Searching
	 ("M-s r" . consult-ripgrep)
	 ("C-s" . consult-line)       ; Alternative: rebind C-s to use
	 ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
	 ("M-s L" . consult-line-multi) ; isearch to M-s s
	 ("M-s o" . consult-outline)
	 ;; Isearch integration
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
	 ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
	 )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

;; Integration between embark and consult
(use-package embark-consult
  :ensure t)

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult)
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun taomacs-avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'taomacs-avy-action-embark))

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
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
  :config
  (setq completion-styles '(orderless)))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("SPC" . corfu-insert-separator)
	("C-n" . corfu-next)
	("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; nerd-icons for completion candidates
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t

  :config
  (defun taomacs-dired-subtree-add-nerd-icons ()
    (interactive)
    (revert-buffer))

  (defun taomacs-dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
      (if nerd-icons-dired-mode
	  (advice-add #'dired-subtree-toggle :after #'taomacs-dired-subtree-add-nerd-icons)
	(advice-remove #'dired-subtree-toggle #'taomacs-dired-subtree-add-nerd-icons))))

  :hook
  ((dired-mode . nerd-icons-dired-mode)
   (nerd-icons-dired-mode . taomacs-dired-subtree-toggle-nerd-icons)))

;; Snippet
(use-package yasnippet
  :ensure t

  :init
  (yas-global-mode 1)

  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; Auto insert a template when opening a file
(use-package autoinsert
  :init
  ;; Don't ask before insertion
  (setopt auto-insert-query nil)

  ;; Set autoinsert's template directory
  (setq auto-insert-directory (locate-user-emacs-file "templates"))

  ;; Enable it
  (auto-insert-mode 1)

  :config
  (defun taomacs-autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (define-auto-insert "\\.el$" ["default-elisp.el" taomacs-autoinsert-yas-expand])

  :hook
  (find-file . auto-insert))

;; Navigation aid
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
	 ("s-j"   . avy-goto-char-timer)))

;; Modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

;; eshell
(use-package eshell
  :init
  (defun taomacs-setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))

  (defun taomacs-toggle-eshell ()
    "Opens an eshell window in the bottom area when there is not one."
    (interactive)
    (let ((eshell-buf (get-buffer "*eshell*")))
      (cond
       ;; Already in eshell window → close it
       ((string= (buffer-name) "*eshell*")
	(delete-window))
       ;; Eshell open somewhere else → jump to it
       ((and eshell-buf (get-buffer-window eshell-buf))
	(select-window (get-buffer-window eshell-buf)))
       ;; Eshell buffer exists but not visible → show it in a split
       (eshell-buf
	(split-window-vertically -15)
	(other-window 1)
	(switch-to-buffer eshell-buf))
       ;; No eshell buffer yet → create one
       (t
	(split-window-vertically -15)
	(other-window 1)
	(eshell)))))

  :bind
  (("C-c t" . taomacs-toggle-eshell))

  :hook
  ((eshell-mode . taomacs-setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")

  :hook
  (;; use Eat to handle term codes in program output
   (eshell-load . eat-eshell-mode)
   ;; commands like less will be handled by Eat
   (eshell-load . eat-eshell-visual-command-mode)))

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Project management
(use-package project
  :config
  (setopt project-vc-extra-root-markers '("deps.edn"))

  :custom
  (when (>= emacs-major-version 30)
    ;; show project name in modeline
    (project-mode-line t)))

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

  (setopt magit-log-section-commit-count 30)

  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; ;; Helpful resources:
;; ;; - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
;; (use-package eglot
;;   ;; no :ensure t here because it's built-in

;;   ;; Configure hooks to automatically turn-on eglot for selected modes
;;   ; :hook
;;   ; (((python-mode ruby-mode elixir-mode) . eglot-ensure))

;;   :custom
;;   (eglot-send-changes-idle-time 0.1)
;;   (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

;;   :config
;;   (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
;;   ;; Sometimes you need to tell Eglot where to find the language server
;;   ; (add-to-list 'eglot-server-programs
;;   ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
;;   )

;; (use-package lsp-mode
;;   :ensure t

;;   :init
;;   (setq lsp-keymap-prefix "C-c l")

;;   (defun taomacs-clojure-mode-config ()
;;     ;; Disable lsp completion
;;     (setq lsp-completion-enable nil)
;;     ;; Disable hover
;;     (setq lsp-eldoc-enable-hover t))

;;   :hook
;;   ((lsp-mode . lsp-enable-which-key-integration)
;;    (clojure-ts-mode . lsp)
;;    (clojure-ts-mode . taomacs-clojure-mode-config))

;;   :commands lsp)

;; crux: provides some useful commands
(use-package crux
  :ensure t)

;; mwim: Move Where I Mean
;; Make moving to beginning or end of a line more like other editors
(use-package mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))

;; Make `forward-word', `backward-word', `backward-kill-word'
;; and `forward-kill-word' less greedy
(use-package bbww
  :ensure t
  :config
  (bbww-mode 1)
  (bbww-init-global-bindings))

;; Copy environment variables into Emacs
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

;; Enhancement for window navigation
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

;; Better interface for Emacs' help
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

;; Indication of local VCS changes
(use-package diff-hl
  :ensure t
  :hook
  ;; Enable `diff-hl' support by default in programming buffers
  (prog-mode . diff-hl-mode)
  :config
  ;; Update the highlighting without saving
  (diff-hl-flydiff-mode t))

;; Clojure major mode that uses Tree-sitter
(use-package clojure-ts-mode
  :ensure t)

;; Clojure REPL
(use-package cider
  :ensure t)

;; Clojure refactoring utilities
;; (use-package clj-refactor
;;   :ensure t
;;   :hook
;;   (clojure-ts-mode-hook . (lambda ()
;;			    (clj-refactor-mode 1)
;;			    (yas-minor-mode 1)
;;			    (cljr-add-keybindings-with-prefix "C-c C-m"))))

;; Emacs support for Common Lisp
(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")

  :hook
  ((slime-mode . (lambda () (setq-local corfu-popupinfo-delay nil)))
   (slime-repl-mode . (lambda () (setq-local corfu-popupinfo-delay nil))))

  :config
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-mrepl)))

;; Provide some helpers for structural editing
;; (use-package smartparens
;;   :ensure t
;;   :hook (prog-mode)
;;   :config
;;   (require 'smartparens-config))

;; Structural editing
;; (use-package structural-editing
;;   :ensure nil
;;   :after smartparens

;;   :init
;;   (defun point-at-beginning-of-sexp ()
;;     (save-excursion
;;       (let ((here (point)))
;;	(ignore-errors
;;	  (forward-sexp)
;;	  (backward-sexp)
;;	  (= here (point))))))

;;   (defun taomacs-drag-sexp-backward ()
;;     (interactive)
;;     (ignore-errors
;;       (if (point-at-beginning-of-sexp)
;;	  (progn
;;	    (transpose-sexps 1)
;;	    (backward-sexp 2))
;;	(backward-sexp)
;;	(transpose-sexps 1)
;;	(backward-sexp 2))))

;;   (defun taomacs-drag-sexp-forward ()
;;     (interactive)
;;     (condition-case _
;;	(progn
;;	  (forward-sexp)
;;	  (transpose-sexps 1)
;;	  (backward-sexp))
;;       (scan-error (backward-sexp))))

;;   (defvar structural-edit-map
;;     (let ((map (make-sparse-keymap)))
;;       (pcase-dolist (`(,k . ,f)
;;		     '(("a" . beginning-of-defun)
;;		       ("e" . end-of-defun)
;;		       ("u" . backward-up-list)
;;		       ("d" . down-list)
;;		       ("f" . forward-sexp)
;;		       ("b" . backward-sexp)
;;		       ("n" . sp-next-sexp)
;;		       ("p" . sp-previous-sexp)
;;		       ("[" . taomacs-drag-sexp-backward)
;;		       ("]" . taomacs-drag-sexp-forward)
;;		       ("k" . kill-sexp)
;;		       ("j" . sp-join-sexp)
;;		       ("s" . sp-split-sexp)
;;		       ("w" . sp-splice-sexp)
;;		       ("r" . raise-sexp)
;;		       ("\\" . indent-region)
;;		       ("/" . undo)
;;		       ("t" . transpose-sexps)
;;		       ("x" . eval-defun)))
;;	(define-key map (kbd k) f))
;;       map))

;;   (map-keymap
;;    (lambda (_ cmd)
;;      (put cmd 'repeat-map 'structural-edit-map))
;;    structural-edit-map)

;;   :bind
;;   (("C-M-r" . raise-sexp)
;;    ("C-M-[" . taomacs-drag-sexp-backward)
;;    ("C-M-]" . taomacs-drag-sexp-forward)

;;    ("C-M-n" . sp-next-sexp)
;;    ("C-M-p" . sp-previous-sexp)
;;    ("M-]" . sp-forward-slurp-sexp)
;;    ("M-[" . sp-backward-slurp-sexp)
;;    ("M-}" . sp-forward-barf-sexp)
;;    ("M-{" . sp-backward-barf-sexp)
;;    ("C-M-j" . sp-join-sexp)
;;    ("C-M-s" . sp-split-sexp)
;;    ("C-M-w" . sp-splice-sexp)))

;; SQL client in Emacs
(use-package sql
  :config
  (setq sql-input-ring-file-name (expand-file-name "~/.emacs.d/sql-history"))

  (setq sql-connection-alist
	'((sixsf-local
	   (sql-product 'postgres)
	   (sql-server "127.0.0.1")
	   (sql-port 5433)
	   (sql-database "sixsf")
	   (sql-user "sixsf"))))

  :hook
  (sql-interactive-mode . (lambda ()
			    (setq sql-prompt-regexp "^[-[:alnum:]_]*=[#>] ")
			    (setq sql-prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] ")
			    ;; Truncate long result buffers instead of freezing
			    (setq comint-buffer-maximum-size 5000)
			    (add-hook 'comint-output-filter-functions
				      'comint-truncate-buffer nil t))))

;; Load local configuration in "local.el"
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config)))

(setq gc-cons-threshold (or bedrock--initial-gc-threshold 800000))
