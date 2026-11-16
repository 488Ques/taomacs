;;; taomacs-bootstrap.el --- Package archive bootstrap -*- lexical-binding: t -*-

;;; Code:

(require 'package)
(require 'seq)

(defun taomacs-bootstrap-refresh-package-archives ()
  "Refresh package indexes if any configured archive is missing its cache."
  ;; A partial cache cannot resolve dependencies across all archives.
  (unless (seq-every-p
           (lambda (archive)
             (file-exists-p
              (expand-file-name
               (concat "archives/" (car archive) "/archive-contents")
               package-user-dir)))
           package-archives)
    (package-refresh-contents)))

(provide 'taomacs-bootstrap)
;;; taomacs-bootstrap.el ends here
