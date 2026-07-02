;;; taomacs-help.el --- Help & discoverability -*- lexical-binding: t -*-

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

(provide 'taomacs-help)
;;; taomacs-help.el ends here
