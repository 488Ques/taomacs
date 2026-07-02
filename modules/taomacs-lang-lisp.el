;;; taomacs-lang-lisp.el --- Lisp languages -*- lexical-binding: t -*-

;; Clojure major mode that uses Tree-sitter
(use-package clojure-ts-mode
  :ensure t)

;; Clojure REPL
(use-package cider
  :ensure t)

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

(provide 'taomacs-lang-lisp)
;;; taomacs-lang-lisp.el ends here
