;;; taomacs-shell.el --- Shell integration -*- lexical-binding: t -*-

;; eshell
(use-package eshell
  :init
  (defun taomacs-setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))

  (defun taomacs-toggle-eshell ()
    "Opens an eshell window in the bottom area when there is not one."
    (interactive)
    (let ((eshell-buf (get-buffer "*eshell*")))
      (cond
       ;; Already in eshell window → close it
       ((string= (buffer-name) "*eshell*")
	(delete-window))
       ;; Eshell open somewhere else → jump to it
       ((and eshell-buf (get-buffer-window eshell-buf))
	(select-window (get-buffer-window eshell-buf)))
       ;; Eshell buffer exists but not visible → show it in a split
       (eshell-buf
	(split-window-vertically -15)
	(other-window 1)
	(switch-to-buffer eshell-buf))
       ;; No eshell buffer yet → create one
       (t
	(split-window-vertically -15)
	(other-window 1)
	(eshell)))))

  :bind
  (("C-c t" . taomacs-toggle-eshell))

  :hook
  ((eshell-mode . taomacs-setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")

  :hook
  (;; use Eat to handle term codes in program output
   (eshell-load . eat-eshell-mode)
   ;; commands like less will be handled by Eat
   (eshell-load . eat-eshell-visual-command-mode)))

(provide 'taomacs-shell)
;;; taomacs-shell.el ends here
