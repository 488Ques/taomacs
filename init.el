;; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; Start up time's optimization
(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil) 
(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))
(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)

;; Add melpa repository to Emacs' package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load configuration file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; org-structure-template-alist seems to only work when this is present
(when (version<= "9.2" (org-version))
  (require 'org-tempo))
