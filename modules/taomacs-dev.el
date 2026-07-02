;;; taomacs-dev.el --- Development tooling -*- lexical-binding: t -*-

;; which-key is built into Emacs 30.2 — no :ensure needed
(use-package which-key
  :config
  (which-key-mode))

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
  ;; Sometimes you need to tell Eglot where to find the language server
  ;; (add-to-list 'eglot-server-programs
  ;;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

(provide 'taomacs-dev)
;;; taomacs-dev.el ends here
