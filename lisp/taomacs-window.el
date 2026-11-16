;;; taomacs-window.el --- Window navigation commands -*- lexical-binding: t -*-

;;; Code:

(defun taomacs-previous-window ()
  "Select the previous window, the reverse of `other-window'."
  (interactive)
  (other-window -1))

(provide 'taomacs-window)
;;; taomacs-window.el ends here
