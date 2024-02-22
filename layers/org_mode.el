(use-package org
  :init
  (setq org-indent-indentation-per-level 1)
  :bind
  ("C-c a" . org-agenda)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  ;; (org-mode . (lambda ()
  ;;		(custom-set-faces '(fringe ((t (:background "white")))))
  ;;		(setq left-fringe-width 30
  ;;		      right-fringe-width 30)))
  )

;; Total style conversion for org-mode
;; (use-package org-modern
;;   :hook
;;   ((org-mode . org-modern-mode)))

;; Additional Org-mode related functionality
(use-package org-contrib)
