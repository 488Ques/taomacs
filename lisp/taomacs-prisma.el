;;; taomacs-prisma.el --- Prisma schema editing -*- lexical-binding: t -*-

;;; Code:

(require 'treesit)

;; prisma-ts-mode documents this grammar but does not register it.
(add-to-list 'treesit-language-source-alist
	     '(prisma "https://github.com/victorhqc/tree-sitter-prisma" "v1.6.0"))

(defun taomacs-prisma-ts-mode ()
  "Enable `prisma-ts-mode', offering to install its grammar first."
  (interactive)
  (treesit-ensure-installed 'prisma)
  (prisma-ts-mode))

(defun taomacs-prisma--major-version ()
  "Return the major version of the project's Prisma package, or nil."
  (when-let* ((dir (locate-dominating-file
		    default-directory "node_modules/prisma/package.json")))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "node_modules/prisma/package.json" dir))
      (car (split-string (gethash "version" (json-parse-buffer)) "\\.")))))

;; The language server's schema rules follow Prisma's major version, so
;; run the matching release through mise rather than one global install.
(defun taomacs-prisma-eglot-contact (_interactive _project)
  "Run the Prisma language server matching the project's Prisma version."
  (list "mise" "exec"
	(concat "npm:@prisma/language-server@"
		(or (taomacs-prisma--major-version) "latest"))
	"--" "prisma-language-server" "--stdio"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(prisma-ts-mode . taomacs-prisma-eglot-contact)))

(provide 'taomacs-prisma)
;;; taomacs-prisma.el ends here
