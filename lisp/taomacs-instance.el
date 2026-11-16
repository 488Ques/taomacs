;;; taomacs-instance.el --- Start another Emacs instance -*- lexical-binding: t -*-

;;; Commentary:

;; macOS reuses a running app, so opening Emacs again only raises the
;; running instance.  `taomacs-instance-start' starts a separate one, with
;; its own buffers and session.

;;; Code:

(defun taomacs-instance-start ()
  "Start another Emacs instance."
  (interactive)
  (if (eq system-type 'darwin)
      ;; `open -n' asks for a new instance even when Emacs is running.
      (call-process "open" nil 0 nil "-n" "-a"
                    (expand-file-name "../.." invocation-directory)) ; Emacs.app
    (call-process (expand-file-name invocation-name invocation-directory) nil 0))
  (message "Starting a new Emacs"))

(provide 'taomacs-instance)
;;; taomacs-instance.el ends here
