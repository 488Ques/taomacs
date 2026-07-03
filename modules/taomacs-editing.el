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

;; --- VSCode-style backward delete (replaces the `bbww' package) ---
;; Neither command touches the kill ring, matching VSCode (deleted text does
;; not land on the clipboard).

(defun taomacs-backward-delete-word ()
  "Delete backward one chunk, in the style of the `bbww' package.
A chunk is a maximal run of a single class: whitespace, word/symbol
characters, or punctuation.  Exactly one class is deleted per call, so
it is never greedy and never crosses a class boundary.  In `(foo bar)  |'
it deletes only the trailing whitespace, then `)', then `bar', and so on.
Nothing is saved to the kill ring.  At the beginning of a line it joins
with the previous line."
  (interactive)
  (let ((end (point)))
    (cond
     ((bobp) nil)
     ((bolp) (backward-char))                       ; join with previous line
     (t (pcase (char-syntax (char-before))
	  ((or ?\s ?-) (skip-chars-backward " \t")) ; a run of whitespace
	  ((or ?w ?_)  (skip-syntax-backward "w_")) ; a run of word/symbol chars
	  (_           (skip-syntax-backward "^w_ "))))) ; a run of punctuation
    (delete-region (point) end)))

(defun taomacs-backward-delete-line ()
  "Delete from point back to the beginning of the line, like VSCode's
`deleteAllLeft', without saving to the kill ring.  At the beginning of a
line, join with the previous line."
  (interactive)
  (if (bolp)
      (unless (bobp) (delete-char -1))
    (delete-region (line-beginning-position) (point))))

(global-set-key (kbd "M-DEL") #'taomacs-backward-delete-word)
(global-set-key (kbd "C-<backspace>") #'taomacs-backward-delete-line)

;; --- Re-indent the whole buffer ---
(defun taomacs-indent-buffer ()
  "Re-indent the entire buffer using the current major mode's rules.
Unlike the naive `indent-region' over point-min/point-max, this widens
first so a narrowed buffer is still fully indented, and restores both the
narrowing and point afterward so the cursor does not jump."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (indent-region (point-min) (point-max)))))

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

;; Re-open the current (or another) file as root via TRAMP
(use-package sudo-edit
  :ensure t)

(provide 'taomacs-editing)
;;; taomacs-editing.el ends here
