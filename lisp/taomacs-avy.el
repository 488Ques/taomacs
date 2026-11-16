;;; taomacs-avy.el --- Avy and Embark integration -*- lexical-binding: t -*-

;;; Code:

;; Navigation aid
(defun taomacs-avy-action-embark (pt)
  "Run Embark at Avy target PT, then return to the original window."
  (unwind-protect
      (save-excursion
	(goto-char pt)
	(embark-act))
    (select-window (cdr (ring-ref avy-ring 0))))
  t)

(provide 'taomacs-avy)
;;; taomacs-avy.el ends here
