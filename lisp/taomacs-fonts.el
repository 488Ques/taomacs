;;; taomacs-fonts.el --- Font availability helpers -*- lexical-binding: t -*-

;;; Code:

(defun taomacs-font-exists-p (font)
  "Check if FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(provide 'taomacs-fonts)
;;; taomacs-fonts.el ends here
