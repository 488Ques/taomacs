;;; taomacs-ui.el --- Appearance -*- lexical-binding: t -*-

(load-theme 'modus-operandi-tinted t)

(defun taomacs-font-exists-p (font)
  "Check if FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(when (taomacs-font-exists-p "IBM Plex Mono")
  (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140))

;; Nerd Font glyphs live in private-use areas that IBM Plex Mono does not
;; cover, and macOS font fallback does not reach into them.  `nerd-icons'
;; propertizes its own strings with the family, but plain terminal output
;; (e.g. an oh-my-pi prompt inside Ghostel) gets no such property, so map the
;; ranges in the fontset instead.
(when (taomacs-font-exists-p "Symbols Nerd Font Mono")
  (dolist (range '((#xe000 . #xf8ff)      ; BMP private use: most Nerd Font icons
                   (#xf0000 . #xffffd)))  ; plane 15: Material Design Icons
    (set-fontset-font t range "Symbols Nerd Font Mono" nil 'prepend)))

;; Disable clock
(display-time-mode -1)

;; Modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode))

(provide 'taomacs-ui)
;;; taomacs-ui.el ends here
