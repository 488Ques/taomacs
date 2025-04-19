;;; utils.el --- For one-off packages that can't be categorized cleanly

;; Jump to arbitrary positions
(use-package avy
  :bind
  ("C-c s" . avy-goto-char-timer)
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
  :init
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

;; Use puni-mode only for certain major modes.
(use-package puni
  :defer t
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :bind
  (:map puni-mode-map
	("C-<right>" . puni-slurp-forward)
	("C-<left>" . puni-barf-forward)
	("M-<left>" . puni-slurp-backward)
	("M-<right>" . puni-barf-backward)
	("C-M-SPC" . puni-expand-region)
	("C-M-r" . puni-raise)
	("C-<backspace>" . puni-backward-kill-word)
	("C-M-<backspace>" . puni-forward-kill-word)))

(provide 'utils)
