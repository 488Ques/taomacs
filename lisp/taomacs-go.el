;;; taomacs-go.el --- Go editing and save pipeline -*- lexical-binding: t -*-

;;; Code:

(defun taomacs-go-format-on-save ()
  "Organize Go imports and format the buffer using gopls before saving."
  (when (eglot-managed-p)
    (let ((server (eglot-current-server)))
      (dolist (action (eglot-code-actions (point-min) (point-max)
					  "source.organizeImports"))
	(eglot-execute server action)))
    (eglot-format-buffer)))

(defun taomacs-go-ts-setup ()
  "Set up Go display and gopls-only formatting on save.
Go indents with tabs; render them 4 columns wide without changing
the file's tab characters.  `go-ts-mode-indent-offset' matches
`tab-width' so re-indenting emits one tab per level."
  (setq-local tab-width 4)
  (apheleia-mode -1)
  (add-hook 'before-save-hook #'taomacs-go-format-on-save nil t))

(provide 'taomacs-go)
;;; taomacs-go.el ends here
