(use-package org
  :init
  ;; (setq org-indent-indentation-per-level 1)
  ;; (setq org-reverse-note-order t) ; Newest note first

  (setq org-agenda-files '("~/gtd/inbox.org"
                           "~/gtd/gtd.org"
                           "~/gtd/tickler.org"))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/gtd/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/gtd/tickler.org" "Tickler")
                                 "* %i%?")))
  (setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                             ("~/gtd/someday.org" :maxlevel . 1)
                             ("~/gtd/tickler.org" :maxlevel . 2)))
  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "WAITING(w)"
                                      "NEXT(n)"
                                      "|"
                                      "DONE(d)"
                                      "CANCELLED(c)")))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :hook
  (org-mode . visual-line-mode)
  ;; (org-mode . org-indent-mode)
  )

(use-package org-autolist
  :init
  (setq org-autolist-enable-delete nil)
  :hook
  (org-mode . org-autolist-mode))

;; Additional Org-mode related functionality
(use-package org-contrib)

(provide 'org_mode)
