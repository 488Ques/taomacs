(defun taomacs-backward-delete ()
  "Make backward delete behave similarly to other text editors."
  (interactive)
  (let ((deleted-something nil)
	(at-space (looking-back "[ \t\n]")))
    (while (and (> (point) (point-min))
		(eq (looking-back "[ \t\n]") at-space)
		(not (and deleted-something (looking-back "[.:(){},]"))))
      (backward-delete-char 1)
      (setq deleted-something t))))

(defun taomacs-edit-config ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(defun taomacs-find-file-in-dir (dir)
  "Call `find-file' interactively as if `default-directory' is in DIR."
  (let ((default-directory (file-name-as-directory dir)))
    (call-interactively #'find-file)))

(defun taomacs-find-layer ()
  (interactive)
  (taomacs-find-file-in-dir taomacs-layers-dir))

(defun taomacs-list-layers ()
  (let ((filenames (directory-files taomacs-layers-dir
				    nil
				    directory-files-no-dot-files-regexp)))
    (mapcar (lambda (filename)
	      (replace-regexp-in-string ".el$" "" filename))
	    filenames)))

(defun taomacs-split-window-below-and-switch ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun taomacs-split-window-right-and-switch ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun taomacs-delete-terminal-emulator-windows (terminal-mode)
  "Delete all open terminal emulator windows that are of `TERMINAL-MODE' mode.
If one or more window is deleted, it will return t, otherwise nil."
  (let ((terminal-emulator-exists-p nil))
    (dolist (wd (window-list))
      (let ((wb (window-buffer wd)))
	(with-current-buffer wb
	  (when (eq terminal-mode major-mode)
	    (delete-window wd)
	    (setq terminal-emulator-exists-p t)))))
    terminal-emulator-exists-p))

(defun taomacs-toggle-eat-window (&optional arg)
  "Open a small horizontal eshell window.
If the focused buffer's height is large enough, open in another buffer,
otherwise open it in the current buffer.

If ARG is supplied and there's no open eshell window, open a new eshell session."
  (interactive "P")
  (unless (taomacs-delete-terminal-emulator-windows 'eshell-mode)
    (when (> (window-total-height) 28)
      (let ((wd (split-window-below 32)))
	(select-window wd))
      (if arg
	  (eshell t)
	(eshell)))))

(defun taomacs-buffer-major-mode ()
  "Print the current buffer's major mode."
  (interactive)
  (message "%s" major-mode))

(defvar-keymap resize-window-keymap
  :repeat t
  "h" #'shrink-window-horizontally
  "l" #'enlarge-window-horizontally
  "j" #'shrink-window
  "k" #'enlarge-window)

(defvar-keymap config-keymap
  "e" #'taomacs-edit-config
  "f" #'taomacs-find-layer)

(provide 'custom_stuff)
