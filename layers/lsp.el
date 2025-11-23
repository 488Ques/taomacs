;; A good alternative is eglot, which is built-in
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; Setup lsp-mode to work with corfu
  (defun taomacs/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun taomacs/clojure-mode-config ()
    ;; disable lsp completion
    (setq lsp-completion-enable nil)
    ;; disable lsp-mode showing eldoc during symbol at point
    (setq lsp-eldoc-enable-hover nil))

  :custom
  (lsp-completion-provider :none) ;; corfu is used instead

  :hook
  ((clojure-mode . lsp)
   ;; (clojure-mode . taomacs/clojure-mode-config)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . taomacs/lsp-mode-setup-completion))

  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(provide 'lsp)
