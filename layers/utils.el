;; In-Emacs Terminal Emulation
(use-package eat
  :config
  ;; Close the terminal buffer when the shell terminates.
  (setq eat-kill-buffer-on-exit t)
  ;; Enable mouse-support.
  (setq eat-enable-mouse t))

;; Jump to arbitrary positions
(use-package avy
  :bind
  ("C-c z" . avy-goto-word-1)
  :config
  ;; Jump to any open window or frame
  (setq avy-all-windows 'all-frames))

;; Convenience for switching between windows
(use-package ace-window
  :bind (("C-x o" . ace-window)))

;; Helpful hint for keybinding
(use-package which-key
  :init
  (which-key-mode))

;; Better interface for Emacs' help
(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

;; Edit in super-user mode
(use-package sudo-edit)

;; Copy environment variables into Emacs
(use-package exec-path-from-shell
  :config
  (when (memq (display-graphic-p) '(mac ns x))
    (exec-path-from-shell-initialize)))
