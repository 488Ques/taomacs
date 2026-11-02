;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Enable MELPA
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;;; Editor defaults

;; Core configuration
(use-package emacs
  :config

  (defun taomacs-backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
	   (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
	   (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) t)
      backupFilePath))

  ;; Function to quickly open the init.el file
  (defun taomacs-open-init-file ()
    "Open init.el file for editing."
    (interactive)
    (find-file user-init-file))

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
   ;; By default, don't underline trailing spaces
   show-trailing-whitespace nil
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

;; Smooth pixel scrolling for trackpads and high-resolution mice.
(use-package ultra-scroll
  :ensure t
  :config
  (ultra-scroll-mode 1))

;; Run actions at midnight
;; By defaul clean up unused buffers at midnight
(use-package midnight
  :hook (after-init . midnight-mode))

;; Enhancement for window navigation
(defun taomacs-previous-window ()
  "Select the previous window, the reverse of `other-window'."
  (interactive)
  (other-window -1))

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

(defun taomacs-font-exists-p (font)
  "Check if FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(when (taomacs-font-exists-p "IBM Plex Mono")
  (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))

;; Keep private-use glyph fallback for plain terminal output (e.g. an
;; oh-my-pi prompt inside Ghostel), independently of Emacs UI icons.
(when (taomacs-font-exists-p "Symbols Nerd Font Mono")
  (dolist (range '((#xe000 . #xf8ff)      ; BMP private use: most Nerd Font icons
		   (#xf0000 . #xffffd)))  ; plane 15: Material Design Icons
    (set-fontset-font t range "Symbols Nerd Font Mono" nil 'prepend)))

;; Disable clock
(display-time-mode -1)

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
  ;; Narrowing prefix; with bufferlo, `< o' lists the other tabs' buffers.
  (setq consult-narrow-key "<"))

;; Bufferlo: every frame and tab keeps its own buffer list — the buffers
;; displayed in it.  `C-x b' offers only those; narrow with `< o' to reach
;; buffers from other tabs.  Buffers are never killed by switching tabs.
(use-package bufferlo
  :ensure t
  :custom
  ;; Also keep `C-x <left>'/`C-x <right>' and `other-buffer' within the tab.
  (bufferlo-prefer-local-buffers 'tabs)
  :init
  (bufferlo-mode)
  :config
  (with-eval-after-load 'consult
    (setq consult-buffer-list-function #'bufferlo-local-buffers))
  (defvar-keymap taomacs-bufferlo-keymap
    :doc "Manage the frame/tab-local buffer list."
    "i" #'bufferlo-ibuffer                ; local buffers in ibuffer
    "o" #'bufferlo-ibuffer-orphans        ; buffers in no tab's list
    "f" #'bufferlo-find-buffer-switch     ; jump to the tab holding a buffer
    "r" #'bufferlo-remove                 ; drop a buffer from this tab
    "c" #'bufferlo-clear                  ; drop all but the current buffer
    "p" #'bufferlo-isolate-project        ; keep only this project's buffers
    "k" #'bufferlo-kill-buffers           ; kill this tab's buffers
    "K" #'bufferlo-tab-close-kill-buffers) ; close tab and kill its buffers
  (keymap-global-set "C-c b" taomacs-bufferlo-keymap))

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

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Extra completion-at-point sources, autoloaded when completion requests them.
(use-package cape
  :ensure t
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; Editing and navigation

;; --- Smart line motion (replaces the `mwim' package) ---
(defun taomacs-beginning-of-line ()
  "Move to first non-whitespace char, or to column 0 if already there."
  (interactive "^")
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point))
      (beginning-of-line))))

(global-set-key (kbd "C-a") #'taomacs-beginning-of-line)
(global-set-key (kbd "C-e") #'end-of-line)

;; --- VSCode-style backward delete (replaces the `bbww' package) ---
;; Neither command touches the kill ring, matching VSCode (deleted text does
;; not land on the clipboard).

(defun taomacs-backward-delete-word ()
  "Delete backward one chunk, in the style of the `bbww' package.
A chunk is a maximal run of a single class: whitespace, word/symbol
characters, or punctuation.  Exactly one class is deleted per call, so
it is never greedy and never crosses a class boundary.  In `(foo bar)  |'
it deletes only the trailing whitespace, then `)', then `bar', and so on.
Nothing is saved to the kill ring.  At the beginning of a line it joins
with the previous line."
  (interactive)
  (let ((end (point)))
    (cond
     ((bobp) nil)
     ((bolp) (backward-char))                       ; join with previous line
     (t (pcase (char-syntax (char-before))
	  ((or ?\s ?-) (skip-chars-backward " \t")) ; a run of whitespace
	  ((or ?w ?_)  (skip-syntax-backward "w_")) ; a run of word/symbol chars
	  (_           (skip-syntax-backward "^w_ "))))) ; a run of punctuation
    (delete-region (point) end)))

(defun taomacs-backward-delete-line ()
  "Delete from point back to the beginning of the line, like VSCode's
`deleteAllLeft', without saving to the kill ring.  At the beginning of a
line, join with the previous line."
  (interactive)
  (if (bolp)
      (unless (bobp) (delete-char -1))
    (delete-region (line-beginning-position) (point))))

(global-set-key (kbd "M-DEL") #'taomacs-backward-delete-word)
(global-set-key (kbd "C-<backspace>") #'taomacs-backward-delete-line)

;; --- Re-indent the whole buffer ---
(defun taomacs-indent-buffer ()
  "Re-indent the entire buffer using the current major mode's rules.
Unlike the naive `indent-region' over point-min/point-max, this widens
first so a narrowed buffer is still fully indented, and restores both the
narrowing and point afterward so the cursor does not jump."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (indent-region (point-min) (point-max)))))

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

  (define-auto-insert "\\.el$" ["default-elisp.el" taomacs-autoinsert-yas-expand]))

;; Navigation aid
(defun taomacs-avy-action-embark (pt)
  "Run Embark at Avy target PT, then return to the original window."
  (unwind-protect
      (save-excursion
	(goto-char pt)
	(embark-act))
    (select-window (cdr (ring-ref avy-ring 0))))
  t)

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-line)
	 ("s-j" . avy-goto-char-timer))
  :config
  (setf (alist-get ?. avy-dispatch-alist) #'taomacs-avy-action-embark))

;; Re-open the current (or another) file as root via TRAMP
(use-package sudo-edit
  :ensure t
  :defer t)

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

;;; Sessions

;; Sessions (buffers, window and frame layout) are saved and restored only
;; when asked for: `desktop-save-mode' stays off, so there is no save-on-exit
;; prompt, no autosave timer, and nothing is restored at startup.
;;
;;   C-c s s   save the current session, overwriting the previous one
;;   C-c s r   restore the saved session

;; `desktop' itself is loaded on first use, by the commands below.
(declare-function desktop-release-lock "desktop" (&optional dirname))
(declare-function desktop--get-file-modtime "desktop" ())

(defconst taomacs-session-directory
  (locate-user-emacs-file "desktop/")
  "Directory holding the hand-saved desktop session.")

(use-package desktop
  :defer t
  :custom
  ;; Only ever look at our own session directory.
  (desktop-dirname taomacs-session-directory)
  (desktop-path (list taomacs-session-directory))
  ;; No background autosave: the session file changes when I save it.
  (desktop-auto-save-timeout nil)
  ;; A lock left by a dead Emacs is not a conflict, so don't ask about it.
  (desktop-load-locked-desktop 'check-pid)
  ;; Restore everything eagerly (the `desktop-restore-eager' default).  Lazy
  ;; restore keeps not-yet-created buffers in `desktop-buffer-args-list',
  ;; which `desktop-save' does not serialize: saving before the idle timer
  ;; drains that list would silently shrink the session to what is live.
  ;; Files that moved or disappeared since the save are not worth a prompt.
  (desktop-missing-file-warning nil)
  ;; These reconstruct themselves from the repository, and restore badly.
  (desktop-modes-not-to-save '(tags-table-mode
			       magit-status-mode
			       magit-diff-mode
			       magit-revision-mode
			       magit-process-mode)))

(defun taomacs-session-file ()
  "Return the file `taomacs-session-save' writes."
  (require 'desktop)
  (expand-file-name desktop-base-file-name taomacs-session-directory))

(defun taomacs-session-save ()
  "Save the current session to `taomacs-session-directory'.
Overwrites any previously saved session without asking."
  (interactive)
  (require 'desktop)
  (make-directory taomacs-session-directory t)
  (setq desktop-dirname taomacs-session-directory)
  ;; `desktop-save' asks before overwriting a file this Emacs did not load.
  ;; An explicit save *is* the answer to that question, so tell the conflict
  ;; detection what is currently on disk.
  (when (file-exists-p (taomacs-session-file))
    (desktop--get-file-modtime))
  ;; RELEASE: nothing here relies on owning the session, and holding the lock
  ;; would only leave a stale one behind on exit.
  (desktop-save taomacs-session-directory t)
  (message "Session saved: %s" (taomacs-session-file)))

(defun taomacs-session-restore ()
  "Restore the session saved by `taomacs-session-save'."
  (interactive)
  (unless (file-exists-p (taomacs-session-file))
    (user-error "No saved session in %s" taomacs-session-directory))
  (desktop-read taomacs-session-directory)
  (desktop-release-lock taomacs-session-directory)
  (message "Session restored: %s" (taomacs-session-file)))

(defvar-keymap taomacs-session-keymap
  :doc "Save and restore the desktop session by hand."
  "s" #'taomacs-session-save
  "r" #'taomacs-session-restore)

(keymap-global-set "C-c s" taomacs-session-keymap)

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
  (("C-c T" . taomacs-toggle-eshell))

  :hook
  ((eshell-mode . taomacs-setup-eshell)))

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

  :config
  ;; `M-w' / `C-w' in copy mode run `ghostel-readonly-copy', which copies and
  ;; then exits read-only mode whenever `ghostel-readonly-fast-exit' is on
  ;; (the default) -- hence only one copy per copy-mode entry.  Its exit
  ;; branch reads the variable dynamically, so let-binding it to nil disables
  ;; the exit *only* for copying: `q' / `C-g' / self-insert still exit, and
  ;; the `use-region-p' guard (absent from plain `kill-ring-save') is kept.
  ;; Bound in the shared parent map, which
  ;; `ghostel-readonly-fast-exit-mode-map' inherits.
  (defun taomacs-ghostel-copy-stay ()
    "Copy the region like `ghostel-readonly-copy', but stay in read-only mode."
    (interactive)
    (let ((ghostel-readonly-fast-exit nil))
      (call-interactively #'ghostel-readonly-copy)))
  (keymap-set ghostel-readonly-mode-map "M-w" #'taomacs-ghostel-copy-stay)
  (keymap-set ghostel-readonly-mode-map "C-w" #'taomacs-ghostel-copy-stay)

  :bind
  (("C-c t" . ghostel)                  ; terminal, next to C-c T (eshell)
   :map project-prefix-map
   ("t" . ghostel-project)))            ; C-x p t: terminal in the current project

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
  (setopt project-vc-extra-root-markers '("deps.edn"))

  ;; Show project name in the modeline (Emacs 30+).
  (setopt project-mode-line t))

;; Tree-sitter: prefer the built-in *-ts-mode variants, but only when the
;; language's grammar is actually installed---otherwise fall back to the
;; classic mode instead of throwing a "grammar unavailable" warning.
;; Grammar availability is checked once at startup, not on every file open.
;;
;; Bookkeeping is manual: install a grammar with
;; `M-x treesit-install-language-grammar' (sources below), then restart Emacs
;; so it gets picked up.  This Emacs loads tree-sitter ABI 15, so the grammars'
;; default branches load fine---no revision pinning needed.
(use-package treesit
  ;; built-in---no :ensure
  :config
  (setq treesit-language-source-alist
	'((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	  (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
	  (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
	  (yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
	  (go    . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))))

  ;; Remap classic modes -> tree-sitter modes, grammar permitting.
  (dolist (remap '((bash       . (sh-mode      . bash-ts-mode))
		   (css        . (css-mode     . css-ts-mode))
		   (javascript . (js-mode      . js-ts-mode))
		   (json       . (js-json-mode . json-ts-mode))
		   (json       . (json-mode    . json-ts-mode))
		   (python     . (python-mode  . python-ts-mode))
		   (yaml       . (yaml-mode    . yaml-ts-mode))))
    (when (treesit-ready-p (car remap) t)
      (add-to-list 'major-mode-remap-alist (cdr remap))))

  ;; .ts/.tsx have no classic major mode, so route them straight to ts-mode.
  (dolist (assoc '((typescript . ("\\.ts\\'"  . typescript-ts-mode))
		   (tsx        . ("\\.tsx\\'" . tsx-ts-mode))
		   (go    . ("\\.go\\'"    . go-ts-mode))
		   (gomod . ("/go\\.mod\\'" . go-mod-ts-mode))))
    (when (treesit-ready-p (car assoc) t)
      (add-to-list 'auto-mode-alist (cdr assoc)))))

;; Copy environment variables into Emacs
(use-package exec-path-from-shell
  :ensure t
  :if (display-graphic-p)
  :config
  (add-to-list 'exec-path-from-shell-variables "SSH_AUTH_SOCK")
  (exec-path-from-shell-initialize))

(use-package mise
  :ensure t
  :hook
  (after-init . global-mise-mode))

;; Markdown-mode leaves a mouse binding on Eglot's rendered links, but Eglot
;; removes the Markdown syntax that binding needs to find the URL.
(defun taomacs-eldoc-buttonize-markdown-links (&rest _)
  "Make rendered Markdown links in the ElDoc buffer followable."
  (when (and (boundp 'markdown-mode-mouse-map)
	     (keymapp markdown-mode-mouse-map))
    (with-current-buffer (eldoc-doc-buffer)
      (let ((inhibit-read-only t)
	    (pos (point-min)))
	(while (< pos (point-max))
	  (let* ((end (or (next-single-property-change
			   pos 'help-echo nil (point-max))
			  (point-max)))
		 (echo (get-text-property pos 'help-echo)))
	    (when (and (stringp echo)
		       (eq (get-text-property pos 'keymap)
			   markdown-mode-mouse-map)
		       (string-match "\\(?:\\`\\|\n\\)\\([^\n]+\\)\\'" echo))
	      (buttonize-region pos end #'browse-url
				(match-string 1 echo) echo))
	    (setq pos end)))))))

(with-eval-after-load 'eldoc
  (advice-add #'eldoc--format-doc-buffer :after
	      #'taomacs-eldoc-buttonize-markdown-links))

;; Helpful resources:
;; - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
(use-package eglot
  ;; Emacs 30's bundled Eglot only supports pushed diagnostics.  TypeScript 7
  ;; requires pull diagnostics, supported by Eglot 1.20 and later.
  :ensure t
  :pin gnu
  :defer t
  :init
  (unless (package-installed-p 'eglot '(1 20))
    (package-refresh-contents)
    (package-install 'eglot))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
  ;; Disable event logging through Eglot's public option, not a JSONRPC override.
  (eglot-events-buffer-config '(:size 0 :format full)))

;; Async, cursor-preserving format-on-save.  Per-language formatters are
;; configured in the language sections below via `with-eval-after-load'.
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

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
  (org-hide-emphasis-markers nil)
  (org-pretty-entities nil)
  (org-ellipsis " ▾")
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
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
  ;; Keep manual completion available without automatic popups in Org buffers.
  (org-mode . (lambda () (setq-local corfu-auto nil)))
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o l" . org-store-link)))

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

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

;;;; Go

(defun taomacs-go-format-on-save ()
  "Organize Go imports and format the buffer using gopls before saving."
  (when (eglot-managed-p)
    (let ((server (eglot-current-server)))
      (dolist (action (eglot-code-actions (point-min) (point-max)
					  "source.organizeImports"))
	(eglot-execute server action)))
    (eglot-format-buffer)))

(defun taomacs-go-ts-setup ()
  "Set up Go display and gopls-only formatting on save.
Go indents with tabs; render them 4 columns wide without changing
the file's tab characters.  `go-ts-mode-indent-offset' matches
`tab-width' so re-indenting emits one tab per level."
  (setq-local tab-width 4)
  (apheleia-mode -1)
  (add-hook 'before-save-hook #'taomacs-go-format-on-save nil t))

;; Auto-start Eglot for Go; its built-in mapping uses gopls.  Eglot
;; selects the project root via project.el, while gopls understands go.mod.
(use-package go-ts-mode
  ;; built-in (Emacs 30)---no :ensure
  :hook ((go-ts-mode . eglot-ensure)
	 (go-ts-mode . taomacs-go-ts-setup))
  :custom
  (go-ts-mode-indent-offset 4))

;;;; Web

;; TypeScript 7 ships its own LSP server instead of tsserver.js, which
;; typescript-language-server requires.  Keep Eglot's built-in JS/TS mode
;; grouping (and language IDs) so all those buffers share one server.
(defun taomacs-typescript-eglot-contact (_interactive _project)
  "Use the project's native TypeScript LSP, or the tsserver bridge."
  (let* ((root (locate-dominating-file
		default-directory "node_modules/typescript/package.json"))
	 (tsc (and root (expand-file-name "node_modules/.bin/tsc" root))))
    (if (and tsc
	     (file-executable-p tsc)
	     (not (file-exists-p
		   (expand-file-name "node_modules/typescript/lib/tsserver.js"
				     root))))
	(list tsc "--lsp" "--stdio")
      '("typescript-language-server" "--stdio"))))

;; TypeScript 7 dynamically subscribes to configuration changes even though
;; Eglot does not advertise dynamic registration for them.  Eglot already
;; sends the initial configuration; forward subsequent Emacs setting changes
;; only while the server's registration is active.
(defvar taomacs-typescript-configuration-watchers nil)

(defun taomacs-typescript-unwatch-configuration (server &rest _)
  "Stop forwarding configuration changes to SERVER."
  (when-let ((watcher (alist-get server taomacs-typescript-configuration-watchers)))
    (remove-variable-watcher 'eglot-workspace-configuration watcher)
    (setq taomacs-typescript-configuration-watchers
	  (assq-delete-all server taomacs-typescript-configuration-watchers))))

(with-eval-after-load 'eglot
  (cl-defmethod eglot-register-capability
    (server (_method (eql workspace/didChangeConfiguration)) _id &rest _options)
    (if (equal (plist-get (eglot--server-info server) :name) "typescript-go")
	(unless (assq server taomacs-typescript-configuration-watchers)
	  (let ((watcher
		 (lambda (_symbol _value operation _where)
		   (when (eq operation 'set)
		     (if (jsonrpc-running-p server)
			 (eglot-signal-didChangeConfiguration server)
		       (taomacs-typescript-unwatch-configuration server))))))
	    (push (cons server watcher) taomacs-typescript-configuration-watchers)
	    (add-variable-watcher 'eglot-workspace-configuration watcher)))
      (cl-call-next-method)))

  (cl-defmethod eglot-unregister-capability
    (server (_method (eql workspace/didChangeConfiguration)) _id &rest _options)
    (if (equal (plist-get (eglot--server-info server) :name) "typescript-go")
	(taomacs-typescript-unwatch-configuration server)
      (cl-call-next-method)))

  (advice-add #'eglot-shutdown :before
	      #'taomacs-typescript-unwatch-configuration))

(with-eval-after-load 'eglot
  (let ((entry (cl-find-if
		(lambda (entry)
		  (and (listp (car entry))
		       (member '(typescript-ts-mode :language-id "typescript")
			       (car entry))))
		eglot-server-programs)))
    (when entry
      (setcdr entry #'taomacs-typescript-eglot-contact))))

;; Auto-start Eglot for TS/TSX.  The JS/TS server selection above chooses
;; the project-local TypeScript 7 LSP or the tsserver bridge for older projects.
;; The .ts/.tsx -> *-ts-mode routing is configured in Development tools.
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook        #'eglot-ensure)

;; Format-on-save with the project-local oxfmt (Oxc formatter).  `npx' makes
;; Apheleia prefer node_modules/.bin/oxfmt; `filepath' lets oxfmt infer the
;; parser from the real file name.  oxfmt reads stdin, writes stdout.
(with-eval-after-load 'apheleia
  (setf (alist-get 'oxfmt apheleia-formatters)
	'(npx "oxfmt" "--stdin-filepath" filepath))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'oxfmt)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'oxfmt))

;; Astro (light): clean highlighting of the frontmatter + HTML + JSX mix.
;; No language server, no formatter---these files are edited rarely.
(use-package web-mode
  :ensure t
  :mode "\\.astro\\'")

;;; Databases

;; A lightweight SQL workbench: connection management, interactive REPLs,
;; and dabbrev-based completion for PostgreSQL, MySQL/MariaDB, and SQLite,
;; built on the built-in `sql.el'.

;;;; Connections
;;
;; Define your databases in `local.el' (git-ignored) via
;; `taomacs-sql-connections'.  Passwords are NOT stored here -- put them in
;; ~/.authinfo.gpg.  Example `local.el' entry:
;;
;; (setq taomacs-sql-connections
;;       '((prod-pg    (sql-product 'postgres) (sql-server "db.example.com")
;;                     (sql-port 5432) (sql-user "me") (sql-database "app"))
;;         (local-mysql (sql-product 'mysql) (sql-server "127.0.0.1")
;;                      (sql-port 3306) (sql-user "root") (sql-database "app"))
;;         (local-lite  (sql-product 'sqlite) (sql-database "~/data/app.db"))))
;;
;; Example ~/.authinfo.gpg line:
;;   machine db.example.com port 5432 login me password s3cret

(defvar taomacs-sql-connections nil
  "Database connections in `sql-connection-alist' format.
Set this in `local.el'.  Each entry is (NAME (VAR VALUE) ...), e.g.
 (mydb (sql-product 'postgres) (sql-server \"host\") (sql-user \"me\")
       (sql-database \"db\")).")

(defun taomacs-sql-connect ()
  "Refresh `sql-connection-alist' from `taomacs-sql-connections' and connect.
Reading connections at call time keeps them current even though
`local.el' loads after this configuration."
  (interactive)
  (require 'sql)
  (setq sql-connection-alist taomacs-sql-connections)
  (call-interactively #'sql-connect))

;; Built-in `sql.el' -- REPL, send-region, object listing.  No :ensure.
(use-package sql
  :bind (("C-c d c" . taomacs-sql-connect)      ; connect to a named database
	 ("C-c d b" . sql-show-sqli-buffer)))   ; show/switch the SQLi REPL

;; Pure-Elisp SQL indentation.
(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

;; Completion: let `cape-dabbrev' see schema dumps and REPL output.  By
;; default `cape-dabbrev' only scans same-major-mode buffers
;; (`cape-same-mode-buffers'), but `sql-list-all' dumps object names into a
;; plain `*List ...*' buffer (fundamental-mode) and the REPL is
;; `sql-interactive-mode' -- both a different mode than the `.sql' buffer, so
;; their identifiers would never complete.  Scope dabbrev to SQL-related
;; buffers instead, buffer-locally, so the rest of Emacs keeps the default.
(defun taomacs-sql--dabbrev-buffers ()
  "Return SQL-related buffers for `cape-dabbrev' to scan.
Covers other SQL edit buffers, interactive REPLs, and the
`*List ...*' object listings produced by `sql-list-all'."
  (seq-filter
   (lambda (buf)
     (or (memq (buffer-local-value 'major-mode buf)
	       '(sql-mode sql-interactive-mode))
	 (string-prefix-p "*List " (buffer-name buf))))
   (buffer-list)))

(defun taomacs-sql--setup-completion ()
  "Scope dabbrev completion to SQL-related buffers in the current buffer."
  (setq-local cape-dabbrev-buffer-function #'taomacs-sql--dabbrev-buffers))

(add-hook 'sql-mode-hook #'taomacs-sql--setup-completion)
(add-hook 'sql-interactive-mode-hook #'taomacs-sql--setup-completion)

;; Load machine-local overrides last
(let ((local (locate-user-emacs-file "local.el")))
  (when (file-exists-p local) (load local)))
