;;; Personal configuration -*- lexical-binding: t -*-

;;; Initialize package sources
(require 'package)
(setq package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("stable-melpa" . "https://stable.melpa.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;;; Initialize use-package for old Emacs and Emacs on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Ensure use-package always installs missing packages
(setq use-package-always-ensure t)

;;; Setup directories
(setq taomacs---layers-dir (expand-file-name "layers" user-emacs-directory))

(setq taomacs--layers '(
			"custom_functions"
			"better_default"
			"appearance"
			"completion_framework"
			"lsp"
			"vcs"
			"utils"
			"clojure"
			"common_lisp"
			"org_mode"
			"markdown"
			))

(defun taomacs--load-layer (layer-name)
  "Loads a Taomacs layer"
  (load (expand-file-name (concat layer-name ".el") taomacs---layers-dir)))

(dolist (layer taomacs--layers)
  (taomacs--load-layer layer))
