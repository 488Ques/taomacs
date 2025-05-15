;; Clojure Support
(use-package clojure-mode)

(use-package cider
  :init
  ;; disable cider showing eldoc during symbol at point
  (setq cider-eldoc-display-for-symbol-at-point nil))

(provide 'clojure)
