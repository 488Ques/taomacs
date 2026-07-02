;;; taomacs-dired.el --- Dired file manager -*- lexical-binding: t -*-

;; Dired: file manager
(use-package dired
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-dwim-target t)                  ;; do what I mean
  (setq dired-recursive-copies 'always)       ;; don't ask when copying directories
  (setq dired-create-destination-dirs 'ask)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-make-directory-clickable t)
  (setq dired-mouse-drag-files t)
  (setq dired-kill-when-opening-new-dired-buffer t)   ;; Tidy up open buffers by default
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))
      (when gls
	(setq dired-use-ls-dired t
	      insert-directory-program gls
	      dired-listing-switches "-aBhl  --group-directories-first")))))

;; Toggle a directory to show items inside it
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
	      ("TAB" . dired-subtree-toggle)))

(use-package nerd-icons-dired
  :ensure t

  :config
  (defun taomacs-dired-subtree-add-nerd-icons ()
    (interactive)
    (revert-buffer))

  (defun taomacs-dired-subtree-toggle-nerd-icons ()
    (when (require 'dired-subtree nil t)
      (if nerd-icons-dired-mode
	  (advice-add #'dired-subtree-toggle :after #'taomacs-dired-subtree-add-nerd-icons)
	(advice-remove #'dired-subtree-toggle #'taomacs-dired-subtree-add-nerd-icons))))

  :hook
  ((dired-mode . nerd-icons-dired-mode)
   (nerd-icons-dired-mode . taomacs-dired-subtree-toggle-nerd-icons)))

(provide 'taomacs-dired)
;;; taomacs-dired.el ends here
