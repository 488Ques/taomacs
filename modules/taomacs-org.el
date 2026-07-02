;;; taomacs-org.el --- Comfortable org-mode baseline -*- lexical-binding: t -*-

(use-package org
  ;; built-in — no :ensure
  :init
  (setopt org-directory "~/org"
          org-default-notes-file (expand-file-name "inbox.org" "~/org")
          org-agenda-files (list "~/org"))
  :custom
  ;; Comfort visuals
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-return-follows-link t)
  (org-fontify-quote-and-verse-blocks t)
  ;; Workflow
  (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
  (org-log-done 'time)
  (org-capture-templates
   '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
      "* TODO %?\n  %U")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\n  %U")))
  :bind
  (("C-c o a" . org-agenda)
   ("C-c o c" . org-capture)
   ("C-c o l" . org-store-link)))

(provide 'taomacs-org)
;;; taomacs-org.el ends here
