;;; Guardrail

(when (< emacs-major-version 29)
  (error "Emacs Bedrock only works with Emacs 29 and newer; you have version %s" emacs-major-version))

;;; Enable MELPA packages

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;;; Emacs configuration

;; This function creates nested directories in the backup folder. If
;; instead you would like all backup files in a flat structure, albeit
;; with their full paths concatenated into a filename, then you can
;; use the following configuration:
;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
;;
;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
;;   (setopt backup-directory-alist `(("." . ,backup-dir))))
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
	 (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
	 (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))

(use-package emacs
  :config
  (load-theme 'modus-operandi-tinted)

  ;; Enable imenu to include 'use-package' declarations
  (setopt use-package-enable-imenu-support t)

  (setopt
   ;; Turn off the welcome screen
   inhibit-splash-screen t
   ;; Default mode for the *scratch* buffer
   initial-major-mode 'fundamental-mode
   ;; this information is useless for most
   display-time-default-load-average nil
   ;; Fix archaic defaults
   sentence-end-double-space nil
   ;; Show current line in modeline
   line-number-mode t
   ;; Show column as well
   column-number-mode t
   ;; Prettier underlines
   x-underline-at-descent-line nil
   ;; Make switching buffers more consistent
   switch-to-buffer-obey-display-actions t
   ;; Prettier underlines
   x-underline-at-descent-line nil
   ;; Make switching buffers more consistent
   switch-to-buffer-obey-display-actions t
   ;; By default, don't underline trailing spaces
   show-trailing-whitespace nil
   ;; Show buffer top and bottom in the margin
   indicate-buffer-boundaries 'left
   ;; Enable horizontal scrolling
   mouse-wheel-tilt-scroll t
   mouse-wheel-flip-direction t
   ;; Set a minimum width for line numbers
   display-line-numbers-width 3)

  ;; We won't set these, but they're good to know about
  ;; (setopt indent-tabs-mode nil)
  ;; (setopt tab-width 4)

  (setopt
   auto-revert-avoid-polling t
   ;; Some systems don't do file notifications well; see
   ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
   auto-revert-interval 5
   auto-revert-check-vc-info t)

  ;; Don't litter file system with *~ backup files; put them all inside
  ;; ~/.emacs.d/backup or wherever
  (setopt make-backup-file-name-function 'bedrock--backup-file-name)

  ;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
  (setopt
   ;; Use the minibuffer whilst in the minibuffer
   enable-recursive-minibuffers t
   ;; TAB cycles candidates
   completion-cycle-threshold 1
   ;; Show annotations
   completions-detailed t
   ;; When I hit TAB, try to complete, otherwise, indent
   tab-always-indent 'complete)

  (setopt
   ;; See `C-h v completion-auto-select' for more possible values
   ;; completion-auto-select t
   ;; Different styles to match input to candidates
   completion-styles '(basic initials substring)
   ;; Open completion always; `lazy' another option
   completion-auto-help 'always
   ;; This is arbitrary
   completions-max-height 20
   ;; Display the completions down the screen in one column
   completions-format 'one-column
   ;; Enable grouping of completion candidates
   completions-group t
   ;; Much more eager
   completion-auto-select 'second-tab)

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
	'((yaml-mode . yaml-ts-mode)
	  (bash-mode . bash-ts-mode)
	  (js2-mode . js-ts-mode)
	  (typescript-mode . typescript-ts-mode)
	  (json-mode . json-ts-mode)
	  (css-mode . css-ts-mode)
	  (python-mode . python-ts-mode)))

  ;; Show the tab-bar as soon as tab-bar functions are invoked
  (setopt tab-bar-show 1)
  ;; Add the time to the tab-bar, if visible
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
  (setopt display-time-format "%a %F %T")
  (setopt display-time-interval 1)
  (display-time-mode)

  ;; Automatically reread from disk if the underlying file changes
  (global-auto-revert-mode)

  ;; Save history of minibuffer
  (savehist-mode)

  ;; Move through windows with Ctrl-<arrow keys>
  (windmove-default-keybindings 'control) ; You can use other modifiers here

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  ;; Steady cursor
  (blink-cursor-mode -1)
  ;; Smooth scrolling
  (pixel-scroll-precision-mode)

  ;; Use common keystrokes by default
  (cua-mode)

  ;; For terminal users, make the mouse more useful
  (xterm-mouse-mode 1)

  :hook
  ;; Display line numbers in programming mode
  (prog-mode . display-line-numbers-mode)
  ;; Nice line wrapping when working with text
  (text-mode . visual-line-mode)
  ;; Modes to highlight the current line with
  ((text-mode prog-mode) . hl-line-mode)
  ;; Auto parenthesis matching
  (prog-mode . electric-pair-mode)

  :bind
  (:map minibuffer-mode-map
	("TAB" . minibuffer-complete)))

;;; packages

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Navigation aid
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
	 ("s-j"   . avy-goto-char-timer)))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
	 ;; Drop-in replacements
	 ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
	 ("M-y"   . consult-yank-pop)   ; orig. yank-pop
	 ("M-g i" . consult-imenu)      ; orig. imenu
	 ;; Searching
	 ("M-s r" . consult-ripgrep)
	 ("C-s" . consult-line)       ; Alternative: rebind C-s to use
	 ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
	 ("M-s L" . consult-line-multi) ; isearch to M-s s
	 ("M-s o" . consult-outline)
	 ;; Isearch integration
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
	 ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
	 ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
	 )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :ensure t)

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult)
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
	(save-excursion
	  (goto-char pt)
	  (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
	      ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
	("SPC" . corfu-insert-separator)
	("C-n" . corfu-next)
	("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package project
  :custom
  (when (>= emacs-major-version 30)
    ;; show project name in modeline
    (project-mode-line t)))

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

;; Helpful resources:
;; - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

(use-package tempel
  :ensure t
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
	 ("M-+" . tempel-complete)
	 :map tempel-map
	 ("C-c RET" . tempel-done)
	 ("C-<down>" . tempel-next)
	 ("C-<up>" . tempel-previous)
	 ("M-<down>" . tempel-next)
	 ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (or bedrock--initial-gc-threshold 800000))
