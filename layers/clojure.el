;; Clojure Support
(use-package clojure-mode)

;; REPL
(use-package cider
  :config
  ; disable cider showing eldoc during symbol at point
  (setq cider-eldoc-display-for-symbol-at-point nil))
