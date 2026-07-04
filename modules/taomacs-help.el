;;; taomacs-help.el --- Help & discoverability -*- lexical-binding: t -*-

;; which-key is built into Emacs 30.2 — no :ensure needed
(use-package which-key
  :config
  (which-key-mode))

;; Better interface for Emacs' help
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol))

;; Quickly jump to a config module, picking by its description
(require 'find-func)  ; find-library, find-library-name

(defun taomacs--module-description (name)
  "Return the header description string for module NAME, or nil."
  (when-let* ((file (ignore-errors (find-library-name (symbol-name name)))))
    (with-temp-buffer
      (insert-file-contents file nil 0 200)
      (goto-char (point-min))
      (when (re-search-forward "^;;;.*? --- \\(.*?\\)\\(?: -\\*-\\|$\\)"
			       (line-end-position) t)
	(string-trim (match-string 1))))))

(defun taomacs-find-module ()
  "Browse and open one of the config modules in `taomacs-modules'."
  (interactive)
  (let* ((descriptions
	  (mapcar (lambda (m)
		    (cons (symbol-name m) (taomacs--module-description m)))
		  taomacs-modules))
	 (annotate
	  (lambda (cand)
	    (when-let* ((desc (cdr (assoc cand descriptions))))
	      (concat "  " (propertize desc 'face 'completions-annotations)))))
	 (choice
	  (completing-read
	   "Module: "
	   (lambda (string pred action)
	     (if (eq action 'metadata)
		 `(metadata (annotation-function . ,annotate)
			    (category . taomacs-module))
	       (complete-with-action action descriptions string pred))))))
    (find-library choice)))

;; Browse config modules by description (C-c e e opens init.el, see taomacs-core)
(global-set-key (kbd "C-c e f") #'taomacs-find-module)

(provide 'taomacs-help)
;;; taomacs-help.el ends here
