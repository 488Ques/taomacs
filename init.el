;;; init.el --- Personal configuration -*- lexical-binding: t -*-

;; straight.el initialization
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
	    user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package, new Emacs versions don't need to do this
(straight-use-package 'use-package)

;; Ensure use-package always installs missing packages
(setq straight-use-package-by-default t)

;;; Define layers directory path
(defvar taomacs-layers-dir (expand-file-name "layers" user-emacs-directory))
(add-to-list 'load-path taomacs-layers-dir)

(defvar taomacs-layers '(
			 ;; Editor
			 custom_stuff ; custom elisp code
			 better_default ; default packages' configuration
			 appearance ; UI stuff
			 completion_framework ; packages that deal with completion
			 lsp ; language servers
			 vcs ; version control system
			 utils ; third-party utilility packages

			 ;; Languages
			 clojure
			 common_lisp
			 org_mode
			 markdown
			 web
			 ))

;; Requires all layers
(dolist (layer taomacs-layers)
  (require layer))
