;;; init.el --- Module loader -*- lexical-binding: t -*-

;; Enable MELPA
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(add-to-list 'load-path (locate-user-emacs-file "modules"))

(defvar taomacs-modules
  '(taomacs-core
    taomacs-ui
    taomacs-completion
    taomacs-editing
    taomacs-dired
    taomacs-git
    taomacs-shell
    taomacs-ai
    taomacs-dev
    taomacs-help
    taomacs-org
    taomacs-lang-lisp
    taomacs-lang-data
    taomacs-lang-go
    taomacs-sql)
  "Ordered list of config modules to load.")

(dolist (m taomacs-modules) (require m))

;; Load machine-local overrides last
(let ((local (locate-user-emacs-file "local.el")))
  (when (file-exists-p local) (load local)))
