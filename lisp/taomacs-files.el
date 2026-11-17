;;; taomacs-files.el --- File commands and backup paths -*- lexical-binding: t -*-

;;; Code:

(defun taomacs-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backup-root-dir (concat user-emacs-directory "emacs-backup/"))
         (file-path (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backup-file-path (replace-regexp-in-string "//" "/" (concat backup-root-dir file-path "~") )))
    (make-directory (file-name-directory backup-file-path) t)
    backup-file-path))

(defun taomacs-open-init-file ()
  "Open init.el file for editing."
  (interactive)
  (find-file user-init-file))

(provide 'taomacs-files)
;;; taomacs-files.el ends here
