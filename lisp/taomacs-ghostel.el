;;; taomacs-ghostel.el --- Ghostel copy commands -*- lexical-binding: t -*-

;;; Code:

(require 'ghostel)

;; Keep the existing copy wrapper separate from its key bindings.
(defun taomacs-ghostel-copy-stay ()
  "Copy the region like `ghostel-readonly-copy', but stay in read-only mode."
  (interactive)
  (let ((ghostel-readonly-fast-exit nil))
    (call-interactively #'ghostel-readonly-copy)))

(provide 'taomacs-ghostel)
;;; taomacs-ghostel.el ends here
