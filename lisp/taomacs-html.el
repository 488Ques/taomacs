;;; taomacs-html.el --- HTML abbreviation expansion -*- lexical-binding: t -*-

;;; Code:

(require 'emmet-mode)
(require 'treesit)

(defun taomacs-html--emmet-abbreviation ()
  "Return an Emmet abbreviation at point in HTML markup, if any."
  (when (and (not (bobp)) (not (use-region-p)) (not current-prefix-arg)
             (memq (char-after) '(nil ?\s ?\t ?\n ?<)))
    ;; A child operator (>) can make the HTML parser split an abbreviation
    ;; into text and error nodes. Check the context at its start instead.
    (when-let* ((expr (condition-case nil (emmet-expr-on-line) (scan-error nil)))
                (start (nth 1 expr))
                (node (treesit-node-at start 'html))
                ((equal (treesit-node-type node) "text"))
                ((= start (save-excursion
                            (goto-char (max (line-beginning-position)
                                            (treesit-node-start node)))
                            (skip-chars-forward " \t")
                            (point))))
                ((< start (treesit-node-end node))))
      expr)))

(defun taomacs-html-tab ()
  "Expand an Emmet abbreviation in HTML, or run the usual Tab command."
  (interactive)
  (if-let* ((expr (taomacs-html--emmet-abbreviation))
            (markup (emmet-html-transform (car expr))))
      (progn
        (delete-region (nth 1 expr) (nth 2 expr))
        (emmet-insert-and-flash markup)
        (emmet-reposition-cursor expr))
    (call-interactively #'indent-for-tab-command)))

(provide 'taomacs-html)
;;; taomacs-html.el ends here
