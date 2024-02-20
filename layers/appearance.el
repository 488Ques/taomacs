;; Fully featured and fast modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Icons for completion UI
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Alternative start-up buffer
;; (use-package dashboard
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-items '((recents . 5)
;;			  ;; (projects . 5)
;;			  ))
;;   (setq dashboard-banner-logo-title "T A O M A C S - The abominable Emacs distribution!")
;;   (setq dashboard-startup-banner "~/.emacs.d/seigasama.png")
;;   (setq dashboard-center-content t)
;;   (setq dashboard-show-shortcuts nil)
;;   (setq dashboard-set-init-info t)
;;   (setq dashboard-init-info (format "%d packages loaded in %s"
;;				    (length package-activated-list) (emacs-init-time "%.2f seconds")))
;;   (setq dashboard-set-footer nil)
;;   (setq dashboard-set-navigator nil))
