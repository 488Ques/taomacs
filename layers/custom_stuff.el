(defun taomacs-backward-delete ()
  "A replacement for C-<backspace> with behavior similar to other text editors."
  (interactive)
  (let ((deleted-something nil)
	(at-space (looking-back "[ \t\n]")))
    (while (and (> (point) (point-min))
		(eq (looking-back "[ \t\n]") at-space)
		(not (and deleted-something (looking-back "[.:(){},]"))))
      (backward-delete-char 1)
      (setq deleted-something t))))

(defun taomacs-edit-config ()
  "Open init file"
  (interactive)
  (find-file user-init-file))

(defun taomacs-find-file-in-dir (dir)
  "Call `find-file' interactively as if `default-directory' is in DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (call-interactively #'find-file)))

(defun taomacs-find-layer ()
  (interactive)
  (taomacs-find-file-in-dir taomacs--layers-dir))

(defun taomacs-list-layers ()
  (let ((filenames (directory-files taomacs--layers-dir
				    nil
				    directory-files-no-dot-files-regexp)))
    (mapcar (lambda (filename)
	      (replace-regexp-in-string ".el$" "" filename))
	    filenames)))
