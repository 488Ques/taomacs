;;; early-init.el --- Codes that are executed before `init.el'

;;; Commentary:
;; Code that should be run before the init-file

;;; Code:

;; Improve startup time by pausing garbage collection during init
(setq gc-cons-threshold most-positive-fixnum)

;; Set threshold to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (expt 2 23))))

;; Increase the amount of data Emacs reads from a process
(setq read-process-output-max (* 1024 1024))

;; Disable package.el
(setq package-enable-at-startup nil)

;;; early-init.el ends here
