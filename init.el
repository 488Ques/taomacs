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

;; Auto generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(emmet-mode web-mode dracula which-key use-package swiper sudo-edit smex smart-hungry-delete rainbow-mode rainbow-delimiters powerline page-break-lines magit ido-vertical-mode go-mode flycheck exec-path-from-shell eglot dracula-theme dired-sidebar diminish dashboard company ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "ADBO" :family "Hack  ")))))
