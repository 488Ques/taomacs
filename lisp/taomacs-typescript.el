;;; taomacs-typescript.el --- TypeScript Eglot integration -*- lexical-binding: t -*-

;;; Code:

;; Keep Eglot's built-in JS/TS grouping while selecting the project's server.
(defun taomacs-typescript-eglot-contact (_interactive _project)
  "Use the project's native TypeScript LSP, or the tsserver bridge."
  (let* ((root (locate-dominating-file
                default-directory "node_modules/typescript/package.json"))
         (tsc (and root (expand-file-name "node_modules/.bin/tsc" root))))
    (if (and tsc
             (file-executable-p tsc)
             (not (file-exists-p
                   (expand-file-name "node_modules/typescript/lib/tsserver.js"
                                     root))))
        (list tsc "--lsp" "--stdio")
      '("typescript-language-server" "--stdio"))))

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
