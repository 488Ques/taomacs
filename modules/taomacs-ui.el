;;; taomacs-ui.el --- Appearance -*- lexical-binding: t -*-

(load-theme 'modus-operandi-tinted t)

(defun taomacs-font-exists-p (font)
  "Check if FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(when (taomacs-font-exists-p "IBM Plex Mono")
  (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))

;; Disable clock
(display-time-mode -1)

;; Modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(provide 'taomacs-ui)
;;; taomacs-ui.el ends here
