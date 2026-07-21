;;; taomacs-lang-go.el --- Go language support -*- lexical-binding: t -*-

(defun taomacs-go-ts-setup ()
  "Buffer-local Go tweaks: render tabs 4 wide like VS Code.
Go indents with tabs (one per level); Emacs shows a tab 8 columns
wide by default, so the same file looks twice as indented as in most
other editors.  This is display-only---the file's tab characters are
unchanged.  `go-ts-mode-indent-offset' is kept equal to `tab-width'
so re-indenting emits exactly one tab per level, not two."
  (setq-local tab-width 4))

;; Auto-start eglot for Go.  eglot ships the go-ts-mode -> gopls mapping,
;; and roots a server per go.mod automatically, so each service in the
;; monorepo gets isolated intelligence with no extra config.
(use-package go-ts-mode
  ;; built-in (Emacs 30)---no :ensure
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . taomacs-go-ts-setup))
  :custom
  (go-ts-mode-indent-offset 4))

;; Format-on-save with goimports (gofmt formatting + import management).
;; goimports reads stdin and writes stdout, which is Apheleia's default I/O.
(with-eval-after-load 'apheleia
  (setf (alist-get 'goimports apheleia-formatters) '("goimports"))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports))

(provide 'taomacs-lang-go)
;;; taomacs-lang-go.el ends here
