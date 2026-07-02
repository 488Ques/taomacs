;;; taomacs-git.el --- Git integration -*- lexical-binding: t -*-

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :config
  ;; 1. Replace the "OR" function with the specific "Unpushed" function
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-unpushed-to-upstream
			  'magit-insert-unpushed-to-upstream-or-recent
			  'replace)

  ;; 2. Add the "Recent" function explicitly after the "Unpushed" one
  (magit-add-section-hook 'magit-status-sections-hook
			  'magit-insert-recent-commits
			  'magit-insert-unpushed-to-upstream
			  t)

  (setopt magit-log-section-commit-count 30)

  :bind (("C-x g" . magit-status)))

;; Indication of local VCS changes
(use-package diff-hl
  :ensure t
  :hook
  ;; Enable `diff-hl' support by default in programming buffers
  (prog-mode . diff-hl-mode)
  :config
  ;; Update the highlighting without saving
  (diff-hl-flydiff-mode t))

(provide 'taomacs-git)
;;; taomacs-git.el ends here
