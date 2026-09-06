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

;; Ghostel: terminal emulator powered by libghostty (the Ghostty VT engine).
;; The native module lives in the package dir; `M-x ghostel-download-module'
;; re-fetches it after an upgrade.
(use-package ghostel
  :ensure t

  ;; :init, not :config -- this has to run before ghostel is first loaded,
  ;; otherwise "Ghostel" is missing from the C-x p p menu until then.
  :init
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t))

  :config
  ;; `M-w' / `C-w' in copy mode run `ghostel-readonly-copy', which copies and
  ;; then exits read-only mode whenever `ghostel-readonly-fast-exit' is on
  ;; (the default) -- hence only one copy per copy-mode entry.  Its exit
  ;; branch reads the variable dynamically, so let-binding it to nil disables
  ;; the exit *only* for copying: `q' / `C-g' / self-insert still exit, and
  ;; the `use-region-p' guard (absent from plain `kill-ring-save') is kept.
  ;; Bound in the shared parent map, which
  ;; `ghostel-readonly-fast-exit-mode-map' inherits.
  (defun taomacs-ghostel-copy-stay ()
    "Copy the region like `ghostel-readonly-copy', but stay in read-only mode."
    (interactive)
    (let ((ghostel-readonly-fast-exit nil))
      (call-interactively #'ghostel-readonly-copy)))
  (keymap-set ghostel-readonly-mode-map "M-w" #'taomacs-ghostel-copy-stay)
  (keymap-set ghostel-readonly-mode-map "C-w" #'taomacs-ghostel-copy-stay)

  :bind
  (("C-c T" . ghostel)                  ; a real terminal, next to C-c t (eshell)
   :map project-prefix-map
   ("t" . ghostel-project)))            ; C-x p t: terminal in the current project

(provide 'taomacs-shell)
;;; taomacs-shell.el ends here
