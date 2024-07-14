(use-package emacs
  :bind
  (("C-<backspace>" . taomacs-backward-delete)
   ("C-M-r" . raise-sexp)
   ("C-/" . comment-line)
   ("C-x 2" . split-window-below-and-switch)
   ("C-x 3" . split-window-right-and-switch))

  :bind-keymap
  ("C-c e" . config-keymap)

  :hook
  ;; Clean up whitespace on save
  (before-save . whitespace-cleanup)

  :config
  ;; Load a custom theme
  ;; Requires Emacs 28
  ;; (load-theme 'modus-operandi t)
  ;; Disable splash screen
  (setq inhibit-startup-screen t)
  ;; Set default font face
  (set-face-attribute 'default nil :height 120 :font "IBM Plex Mono")
  ;; Disable the menu bar
  (menu-bar-mode -1)
  ;; Disable the tool bar
  (tool-bar-mode -1)
  ;; Disable the scroll bars
  (scroll-bar-mode -1)
  ;; Automatically pair parentheses
  (electric-pair-mode t)
  ;; Common User Actions - Replace default cut, copy and undo keybinds
  (cua-mode 1)

  ;; Miscellaneous options
  (setq-default major-mode
		(lambda () ; guess major mode from file name
		  (unless buffer-file-name
		    (let ((buffer-file-name (buffer-name)))
		      (set-auto-mode)))))
  ;; Ask before closing Emacs
  (setq confirm-kill-emacs #'y-or-n-p)
  (setq window-resize-pixelwise t)
  (setq frame-resize-pixelwise t)
  (save-place-mode t)
  (savehist-mode t)
  (recentf-mode t)
  ;; yes/no to y/n
  (setq use-short-answers t)
  ;; Display all non-nil results of functions in the eldoc hook
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose)

  ;; Keymaps
  (defvar config-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map "e" #'taomacs-edit-config)
      (define-key map "f" #'taomacs-find-layer)
      map)
    "Emacs config related keymap")

  ;; Store automatic customisation options elsewhere
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Relocate all backup files into one directory
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))))

;; Inline static analysis
(use-package flymake
  :hook
  ;; Enabled inline static analysis for programming mode
  (prog-mode . flymake-mode)
  :init
  ;; Display messages when idle, without prompting
  (setq help-at-pt-display-when-idle t))

;; Workspace
(use-package tab-bar
  :bind
  (("C-c t t" . tab-new)
   ("C-c t k" . tab-close)
   ("C-<tab>" . tab-next)
   ("C-<next>" . tab-next)
   ("C-S-<tab>" . tab-previous)
   ("C-<prior>" . tab-previous))
  ;; :config
  ;; (tab-bar-mode 1)
  )
