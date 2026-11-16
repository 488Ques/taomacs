;;; taomacs-files.el --- File commands and backup paths -*- lexical-binding: t -*-

;;; Code:

(defun taomacs-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) t)
    backupFilePath))

(defun taomacs-open-init-file ()
  "Open init.el file for editing."
  (interactive)
  (find-file user-init-file))

(provide 'taomacs-files)
;;; taomacs-files.el ends here
