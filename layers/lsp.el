;; A good alternative is lsp-mode
(use-package eglot
  ;; :hook
  ;; Enable LSP support by default in programming modes of your choice
  ;; (foo-mode . eglot-ensure)
  :config
  ;; Create a memorable alias for `eglot-ensure'.
  (defalias 'start-lsp-server #'eglot))
