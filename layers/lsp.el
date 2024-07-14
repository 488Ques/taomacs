;; A good alternative is
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun taomacs/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless)))
  :custom
  (lsp-completion-provider :none)
  :hook
  ((clojure-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-completion-mode . taomacs/lsp-mode-setup-completion))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)
