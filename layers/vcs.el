;; Git porcelain
(use-package magit
  :bind
  ;; Bind the `magit-status' command to a convenient key.
  ("C-c g" . magit-status)
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t))

;; Indication of local VCS changes
(use-package diff-hl
  :hook
  ;; Enable `diff-hl' support by default in programming buffers
  (prog-mode . diff-hl-mode)
  :config
  ;; Update the highlighting without saving
  (diff-hl-flydiff-mode t))
