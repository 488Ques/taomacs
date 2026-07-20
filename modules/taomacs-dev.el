;;; taomacs-dev.el --- Development tooling -*- lexical-binding: t -*-

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; Project management
(use-package project
  :config
  (setopt project-vc-extra-root-markers '("deps.edn"))

  (when (>= emacs-major-version 30)
    ;; show project name in modeline
    (setopt project-mode-line t)))

;; Tree-sitter: prefer the built-in *-ts-mode variants, but only when the
;; language's grammar is actually installed---otherwise fall back to the
;; classic mode instead of throwing a "grammar unavailable" warning.  This is
;; a plain static setup that runs once at startup, so there is no per-file-open
;; cost (an earlier `treesit-auto' + `global-treesit-auto-mode' setup re-probed
;; every grammar on each file open, adding ~100ms).
;;
;; Bookkeeping is manual: install a grammar with
;; `M-x treesit-install-language-grammar' (sources below), then restart Emacs
;; so it gets picked up.  This Emacs loads tree-sitter ABI 15, so the grammars'
;; default branches load fine---no revision pinning needed.
(use-package treesit
  ;; built-in---no :ensure
  :config
  (setq treesit-language-source-alist
	'((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
	  (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
	  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	  (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
	  (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
	  (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
	  (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
	  (yaml       . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
	  (go    . ("https://github.com/tree-sitter/tree-sitter-go"))
	  (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))))

  ;; Remap classic modes -> tree-sitter modes, grammar permitting.
  (dolist (remap '((bash       . (sh-mode      . bash-ts-mode))
		   (css        . (css-mode     . css-ts-mode))
		   (javascript . (js-mode      . js-ts-mode))
		   (json       . (js-json-mode . json-ts-mode))
		   (json       . (json-mode    . json-ts-mode))
		   (python     . (python-mode  . python-ts-mode))
		   (yaml       . (yaml-mode    . yaml-ts-mode))))
    (when (treesit-ready-p (car remap) t)
      (add-to-list 'major-mode-remap-alist (cdr remap))))

  ;; .ts/.tsx have no classic major mode, so route them straight to ts-mode.
  (dolist (assoc '((typescript . ("\\.ts\\'"  . typescript-ts-mode))
		   (tsx        . ("\\.tsx\\'" . tsx-ts-mode))
		   (go    . ("\\.go\\'"    . go-ts-mode))
		   (gomod . ("/go\\.mod\\'" . go-mod-ts-mode))))
    (when (treesit-ready-p (car assoc) t)
      (add-to-list 'auto-mode-alist (cdr assoc)))))

;; Copy environment variables into Emacs
(use-package exec-path-from-shell
  :ensure t
  :init
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

(use-package mise
  :ensure t
  :hook
  (after-init . global-mise-mode))

;; Helpful resources:
;; - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ;; :hook
  ;; (((python-mode ruby-mode elixir-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  )

;; Async, cursor-preserving format-on-save.  Per-language formatters are
;; registered in the taomacs-lang-* modules via `with-eval-after-load'.
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(provide 'taomacs-dev)
;;; taomacs-dev.el ends here
