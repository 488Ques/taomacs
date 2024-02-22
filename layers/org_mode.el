(use-package org
  :bind
  ("C-c a" . org-agenda)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . (lambda ()
		(custom-set-faces '(fringe ((t (:background "white")))))
		(setq left-fringe-width 30
		      right-fringe-width 30))))

(use-package org-modern
  :hook
  ((org-mode . org-modern-mode)))

;; Additional Org-mode related functionality
(use-package org-contrib)
