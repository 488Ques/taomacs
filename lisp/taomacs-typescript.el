;;; taomacs-typescript.el --- TypeScript Eglot integration -*- lexical-binding: t -*-

;;; Code:

(defun taomacs-typescript--project-file (file)
  "Return FILE's absolute name in the nearest directory that has it."
  (when-let* ((dir (locate-dominating-file default-directory file)))
    (expand-file-name file dir)))

(defun taomacs-typescript--linter ()
  "Return the linter the project installs: `eslint', `oxlint', or nil."
  (cond ((taomacs-typescript--project-file "node_modules/.bin/eslint") 'eslint)
        ((taomacs-typescript--project-file "node_modules/.bin/oxlint") 'oxlint)))

(defun taomacs-typescript--typescript-server ()
  "Return the project's native TypeScript LSP, or the tsserver bridge."
  (let* ((root (locate-dominating-file
                default-directory "node_modules/typescript/package.json"))
         (tsc (and root (expand-file-name "node_modules/.bin/tsc" root))))
    (if (and tsc
             (file-executable-p tsc)
             (not (file-exists-p
                   (expand-file-name "node_modules/typescript/lib/tsserver.js"
                                     root))))
        (list tsc "--lsp" "--stdio")
      (list "typescript-language-server" "--stdio"))))

(defun taomacs-typescript--companion-servers ()
  "Return the linter and Tailwind servers matching the project's packages."
  (delq nil
        (list (pcase (taomacs-typescript--linter)
                ('eslint (list "vscode-eslint-language-server" "--stdio"))
                ('oxlint (list (taomacs-typescript--project-file
                                "node_modules/.bin/oxlint")
                               "--lsp")))
              (and (taomacs-typescript--project-file
                    "node_modules/tailwindcss/package.json")
                   (list "tailwindcss-language-server" "--stdio")))))

(defconst taomacs-typescript--rass-preset
  (expand-file-name "taomacs-typescript-rass.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "rass preset that supplies ESLint's VS Code settings.")

;; Eglot runs one server per buffer, so rass multiplexes TypeScript with
;; the project's linter and Tailwind.
(defun taomacs-typescript-eglot-contact (_interactive _project)
  "Return the Eglot command for the project's TypeScript servers."
  (let ((servers (cons (taomacs-typescript--typescript-server)
                       (taomacs-typescript--companion-servers))))
    (cond ((null (cdr servers)) (car servers))
          ((executable-find "rass")
           (append (list "rass" "--log-level" "warn" taomacs-typescript--rass-preset)
                   (mapcan (lambda (server) (cons "--" (copy-sequence server)))
                           servers)))
          (t
           (message "rass not found; starting TypeScript without linter or Tailwind")
           (car servers)))))

(defun taomacs-typescript-add-missing-imports ()
  "Import every unresolved name in the buffer."
  (interactive)
  (eglot-code-actions (point-min) (point-max) "source.addMissingImports.ts" t))

(defun taomacs-typescript-fix-all ()
  "Apply every automatic fix from the project's linter."
  (interactive)
  (eglot-code-actions (point-min) (point-max)
                      (pcase (taomacs-typescript--linter)
                        ('eslint "source.fixAll.eslint")
                        ('oxlint "source.fixAll.oxc")
                        (_ (user-error "No ESLint or oxlint in this project")))
                      t))

(with-eval-after-load 'typescript-ts-mode
  (keymap-set typescript-ts-base-mode-map "C-c l o"
              #'eglot-code-action-organize-imports)
  (keymap-set typescript-ts-base-mode-map "C-c l i"
              #'taomacs-typescript-add-missing-imports)
  (keymap-set typescript-ts-base-mode-map "C-c l f" #'taomacs-typescript-fix-all))

(defun taomacs-typescript-select-formatter ()
  "Format with oxfmt in projects that install it; otherwise keep Prettier."
  (when (taomacs-typescript--project-file "node_modules/.bin/oxfmt")
    (setq-local apheleia-formatter 'oxfmt)))

;; TypeScript 7 dynamically subscribes to configuration changes even though
;; Eglot does not advertise dynamic registration for them.  Eglot already
;; sends the initial configuration; forward subsequent Emacs setting changes
;; only while the server's registration is active.
(defvar taomacs-typescript-configuration-watchers nil)

(defun taomacs-typescript-unwatch-configuration (server &rest _)
  "Stop forwarding configuration changes to SERVER."
  (when-let* ((watcher (alist-get server taomacs-typescript-configuration-watchers)))
    (remove-variable-watcher 'eglot-workspace-configuration watcher)
    (setq taomacs-typescript-configuration-watchers
	  (assq-delete-all server taomacs-typescript-configuration-watchers))))

(with-eval-after-load 'eglot
  (cl-defmethod eglot-register-capability
    (server (_method (eql workspace/didChangeConfiguration)) _id &rest _options)
    (if (equal (plist-get (eglot--server-info server) :name) "typescript-go")
	(unless (assq server taomacs-typescript-configuration-watchers)
	  (let ((watcher
		 (lambda (_symbol _value operation _where)
		   (when (eq operation 'set)
		     (if (jsonrpc-running-p server)
			 (eglot-signal-didChangeConfiguration server)
		       (taomacs-typescript-unwatch-configuration server))))))
	    (push (cons server watcher) taomacs-typescript-configuration-watchers)
	    (add-variable-watcher 'eglot-workspace-configuration watcher)))
      (cl-call-next-method)))

  (cl-defmethod eglot-unregister-capability
    (server (_method (eql workspace/didChangeConfiguration)) _id &rest _options)
    (if (equal (plist-get (eglot--server-info server) :name) "typescript-go")
	(taomacs-typescript-unwatch-configuration server)
      (cl-call-next-method)))

  (advice-add #'eglot-shutdown :before
	      #'taomacs-typescript-unwatch-configuration))

(with-eval-after-load 'eglot
  (let ((entry (cl-find-if
		(lambda (entry)
		  (and (listp (car entry))
		       (member '(typescript-ts-mode :language-id "typescript")
			       (car entry))))
		eglot-server-programs)))
    (when entry
      (setcdr entry #'taomacs-typescript-eglot-contact))))

(provide 'taomacs-typescript)
;;; taomacs-typescript.el ends here
