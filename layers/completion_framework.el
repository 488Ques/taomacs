;; Better completion UI
(use-package vertico
  :init
  ;; Enable completion by narrowing
  (vertico-mode t)
  :bind (:map vertico-map
	      ;; Improve directory navigation
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)))

;; Fuzzy completion
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles basic partial-completion)))))

;; Extended completion utilities
(use-package consult
  :bind
  (([rebind switch-to-buffer] . consult-buffer)
   ("C-s" . consult-line)
   ("C-c i" . consult-imenu)
   ("C-c l" . consult-flymake))
  :config
  (setq read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
	completion-ignore-case t))

;; Pop-up completion
(use-package corfu
  :hook
  ;; Enable autocompletion by default in programming buffers
  (prog-mode . corfu-mode)
  :config
  ;; Enable automatic completion
  (setq corfu-auto t)
  ;; Provide popup documentation
  (corfu-popupinfo-mode))

;; Enable rich annotations
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
