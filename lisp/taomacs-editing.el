;;; taomacs-editing.el --- Editing commands -*- lexical-binding: t -*-

;;; Code:

;; --- Smart line motion (replaces the `mwim' package) ---
(defun taomacs-beginning-of-line ()
  "Move to first non-whitespace char, or to column 0 if already there."
  (interactive "^")
  (let ((start (point)))
    (back-to-indentation)
    (when (= start (point))
      (beginning-of-line))))
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

(provide 'taomacs-editing)
;;; taomacs-editing.el ends here
