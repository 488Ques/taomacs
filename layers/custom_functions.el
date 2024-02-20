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
  "Open init.el quickly"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun taomacs-find-file-in-dir (dir)
  "Call `find-file' interactively as if `default-directory' is in DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (call-interactively #'find-file)))

(defun taomacs-find-layer ()
  (interactive)
  (taomacs-find-file-in-dir (expand-file-name "layers/" user-emacs-directory)))
