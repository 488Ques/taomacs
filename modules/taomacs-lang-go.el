;;; taomacs-lang-go.el --- Go language support -*- lexical-binding: t -*-

;; Auto-start eglot for Go.  eglot ships the go-ts-mode -> gopls mapping,
;; and roots a server per go.mod automatically, so each service in the
;; monorepo gets isolated intelligence with no extra config.
(use-package go-ts-mode
  ;; built-in (Emacs 30)---no :ensure
  :hook (go-ts-mode . eglot-ensure))

;; Format-on-save with goimports (gofmt formatting + import management).
;; goimports reads stdin and writes stdout, which is Apheleia's default I/O.
(with-eval-after-load 'apheleia
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports))

(provide 'taomacs-lang-go)
;;; taomacs-lang-go.el ends here
