;; Clojure Support
(use-package clojure-mode)

(use-package cider
  :init
  ;; Disable cider showing eldoc during symbol at point
  ;; (setq cider-eldoc-display-for-symbol-at-point nil)
  (defun taomacs/cider-disable-eldoc ()
    "Let LSP handle ElDoc instead of CIDER."
    (remove-hook 'eldoc-documentation-functions #'cider-eldoc t))
  (defun taomacs/cider-disable-completion ()
    "Let LSP handle completion instead of CIDER."
    (remove-hook 'completion-at-point-functions #'cider-complete-at-point t))

  ;; :hook
  ;; (cider-mode . taomacs/cider-disable-eldoc)
  ;; (cider-mode . taomacs/cider-disable-completion)
  )

(provide 'clojure)
