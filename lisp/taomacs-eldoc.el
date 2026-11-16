;;; taomacs-eldoc.el --- Followable ElDoc Markdown links -*- lexical-binding: t -*-

;;; Code:

;; Markdown-mode leaves a mouse binding on Eglot's rendered links, but Eglot
;; removes the Markdown syntax that binding needs to find the URL.
(defun taomacs-eldoc-buttonize-markdown-links (&rest _)
  "Make rendered Markdown links in the ElDoc buffer followable."
  (when (and (boundp 'markdown-mode-mouse-map)
	     (keymapp markdown-mode-mouse-map))
    (with-current-buffer (eldoc-doc-buffer)
      (let ((inhibit-read-only t)
	    (pos (point-min)))
	(while (< pos (point-max))
	  (let* ((end (or (next-single-property-change
			   pos 'help-echo nil (point-max))
			  (point-max)))
		 (echo (get-text-property pos 'help-echo)))
	    (when (and (stringp echo)
		       (eq (get-text-property pos 'keymap)
			   markdown-mode-mouse-map)
		       (string-match "\\(?:\\`\\|\n\\)\\([^\n]+\\)\\'" echo))
	      (buttonize-region pos end #'browse-url
				(match-string 1 echo) echo))
	    (setq pos end)))))))

(with-eval-after-load 'eldoc
  (advice-add #'eldoc--format-doc-buffer :after
	      #'taomacs-eldoc-buttonize-markdown-links))

(provide 'taomacs-eldoc)
;;; taomacs-eldoc.el ends here
