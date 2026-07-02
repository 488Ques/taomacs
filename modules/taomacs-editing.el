;;; taomacs-editing.el --- Editing helpers -*- lexical-binding: t -*-

;; --- Smart line motion (replaces the `mwim' package) ---
(defun taomacs-beginning-of-line ()
  "Move to first non-whitespace char, or to column 0 if already there."
  (interactive "^")
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point))
      (beginning-of-line))))

(defun taomacs-end-of-line ()
  "Move to end of line (kept for symmetry with `taomacs-beginning-of-line')."
  (interactive "^")
  (end-of-line))

(global-set-key (kbd "C-a") #'taomacs-beginning-of-line)
(global-set-key (kbd "C-e") #'taomacs-end-of-line)

;; --- VSCode-style backward word delete (replaces the `bbww' package) ---
;; Deletes the previous word WITHOUT saving it to the kill ring.
(defun taomacs-backward-delete-word (arg)
  "Delete ARG words backward without saving to the kill ring."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "M-DEL") #'taomacs-backward-delete-word)
(global-set-key (kbd "C-<backspace>") #'taomacs-backward-delete-word)

;; Snippet
(use-package yasnippet
  :ensure t

  :init
  (yas-global-mode 1)

  :config
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets")))

;; Auto insert a template when opening a file
(use-package autoinsert
  :init
  ;; Don't ask before insertion
  (setopt auto-insert-query nil)

  ;; Set autoinsert's template directory
  (setq auto-insert-directory (locate-user-emacs-file "templates"))

  ;; Enable it
  (auto-insert-mode 1)

  :config
  (defun taomacs-autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  (define-auto-insert "\\.el$" ["default-elisp.el" taomacs-autoinsert-yas-expand])

  :hook
  (find-file . auto-insert))

;; Navigation aid
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
	 ("s-j"   . avy-goto-char-timer)))

(provide 'taomacs-editing)
;;; taomacs-editing.el ends here
