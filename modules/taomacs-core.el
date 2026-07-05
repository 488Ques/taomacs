;;; taomacs-core.el --- Core editor defaults -*- lexical-binding: t -*-

;; Core configuration
(use-package emacs
  :config

  ;; This function creates nested directories in the backup folder. If
  ;; instead you would like all backup files in a flat structure, albeit
  ;; with their full paths concatenated into a filename, then you can
  ;; use the following configuration:
  ;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
  ;;
  ;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
  ;;   (setopt backup-directory-alist `(("." . ,backup-dir))))
  (defun taomacs-backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
	   (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
	   (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) t)
      backupFilePath))

  ;; Function to quickly open the init.el file
  (defun taomacs-open-init-file ()
    "Open init.el file for editing."
    (interactive)
    (find-file user-init-file))

  ;; Word wrap
  (global-visual-line-mode 1)

  ;; Enable imenu to include 'use-package' declarations
  (setopt use-package-enable-imenu-support t)

  (setopt
   ;; Turn off the welcome screen
   inhibit-splash-screen t
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
   ;; By default, don't underline trailing spaces
   show-trailing-whitespace nil
   ;; Show buffer top and bottom in the margin
   indicate-buffer-boundaries 'left
   ;; Enable horizontal scrolling
   mouse-wheel-tilt-scroll t
   mouse-wheel-flip-direction t
   ;; Set a minimum width for line numbers
   display-line-numbers-width 3
   ;; y/n instead of yes/no when prompted
   use-short-answers t)

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
  (setopt make-backup-file-name-function 'taomacs-backup-file-name)

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
   ;; completion-styles '(basic initials substring)
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

  ;; Banish the Custom stuff
  (setopt custom-file (locate-user-emacs-file "custom.el"))

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

  ;; For terminal users, make the mouse more useful
  (xterm-mouse-mode 1)

  ;; Auto parenthesis matching
  (electric-pair-mode 1)

  ;; Remember and restore the last cursor location of opened files
  (save-place-mode 1)

  ;; Unset the annoying zooming in behavior when pinching on a touchpad
  (global-unset-key (kbd "<pinch>"))

  ;; Unset this key since I have a habit of pressing it for undo
  (global-unset-key (kbd "C-z"))

  ;; Repeat keybinding
  (repeat-mode 1)

  (defvar-keymap taomacs-resize-window-keymap
    :repeat t
    "h" #'shrink-window-horizontally
    "l" #'enlarge-window-horizontally
    "j" #'shrink-window
    "k" #'enlarge-window)

  ;; Setup list of times to track
  (setopt world-clock-list '(("Asia/Ho_Chi_Minh" "Vietnam")
			     ("Poland" "Poland")
			     ("Portugal" "Portugal")
			     ("America/New_York" "US EST")))

  :hook
  ;; Display line numbers in programming mode
  (prog-mode . display-line-numbers-mode)
  ;; Nice line wrapping when working with text
  (text-mode . visual-line-mode)
  ;; Modes to highlight the current line with
  ((text-mode prog-mode) . hl-line-mode)
  ;; Clean up whitespace
  (before-save . whitespace-cleanup)

  :bind
  (("C-c e e" . taomacs-open-init-file)

   ("C-;" . comment-line)

   ("M-n" . scroll-up-line)
   ("M-p" . scroll-down-line)

   :map minibuffer-mode-map
   ("TAB" . minibuffer-complete))

  :bind-keymap
  ("C-c r" . taomacs-resize-window-keymap))

;; Run actions at midnight
;; By defaul clean up unused buffers at midnight
(use-package midnight
  :hook (after-init . midnight-mode))

;; Enhancement for window navigation
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

;; mini-GCMH: generous GC threshold during activity, collect when idle.
(setq gc-cons-threshold (* 128 1024 1024))
(run-with-idle-timer 5 t #'garbage-collect)

(provide 'taomacs-core)
;;; taomacs-core.el ends here
