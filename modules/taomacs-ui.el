;;; taomacs-ui.el --- Appearance -*- lexical-binding: t -*-

(load-theme 'modus-operandi-tinted t)

(defun taomacs-font-exists-p (font)
  "Check if FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(when (taomacs-font-exists-p "IBM Plex Mono")
  (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))

;; tab-bar + clock (copied from init.el.orig)
(setopt tab-bar-show 1)
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T"
        display-time-interval 1)
(display-time-mode)

;; Modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(provide 'taomacs-ui)
;;; taomacs-ui.el ends here
