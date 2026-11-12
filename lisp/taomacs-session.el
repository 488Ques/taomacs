;;; taomacs-session.el --- Named desktop.el sessions -*- lexical-binding: t -*-

;;; Commentary:

;; A session is a named desktop saved in its own directory under
;; `taomacs-session-directory'.  At most one session is active; while one
;; is, `desktop-save-mode' saves it when idle, on switching, and on exit.
;; Switching saves the active session, kills its buffers (`desktop-clear'),
;; and restores the other session's buffers, frames, tabs, and windows.
;; Ghostel restores its terminals through its own desktop handler; this
;; file adds one for Eshell.

;;; Code:

(require 'desktop)

(defgroup taomacs-session nil
  "Named desktop.el sessions."
  :group 'desktop)

(defcustom taomacs-session-directory (locate-user-emacs-file "sessions/")
  "Directory holding one desktop directory per session."
  :type 'directory)

(defun taomacs-session--dir (name)
  "Return the desktop directory of session NAME."
  (file-name-as-directory (expand-file-name name taomacs-session-directory)))

(defun taomacs-session-names ()
  "Return the names of saved sessions."
  (when (file-directory-p taomacs-session-directory)
    (seq-filter (lambda (name)
                  (file-exists-p (expand-file-name desktop-base-file-name
                                                   (taomacs-session--dir name))))
                (directory-files taomacs-session-directory nil
                                 directory-files-no-dot-files-regexp))))

(defun taomacs-session-current ()
  "Return the active session's name, or nil."
  (and desktop-save-mode desktop-dirname
       (file-name-nondirectory (directory-file-name desktop-dirname))))

(defun taomacs-session--read (prompt &optional existing)
  "Read a session name with PROMPT.
EXISTING non-nil requires the name of a saved session."
  (let ((names (taomacs-session-names)))
    (when (and existing (null names))
      (user-error "No saved sessions"))
    (let ((name (string-trim (completing-read prompt names nil existing))))
      (when (or (string-empty-p name) (string-search "/" name)
                (member name '("." "..")))
        (user-error "Invalid session name: %S" name))
      name)))

(defun taomacs-session--detach ()
  "Stop saving the active session and release its lock."
  (when (and desktop-dirname (eql (desktop-owner) (emacs-pid)))
    (desktop-release-lock))
  (desktop-save-mode -1)
  (setq desktop-dirname nil))

(defun taomacs-session-save (name)
  "Save frames, tabs, windows, and buffers as session NAME and activate it.
Interactively, save the active session; with a prefix argument, or
when no session is active, read NAME."
  (interactive
   (list (or (and (not current-prefix-arg) (taomacs-session-current))
             (taomacs-session--read "Save session as: "))))
  (let ((old desktop-dirname)
        (dir (taomacs-session--dir name)))
    (make-directory dir t)
    ;; `desktop-save' sets `desktop-dirname' before asking whether to
    ;; overwrite a desktop it did not load; stay on OLD if that is refused.
    (condition-case err
        (desktop-save dir)
      ((error quit)
       (setq desktop-dirname old)
       (signal (car err) (cdr err))))
    (when (and old (not (equal old dir)) (eql (desktop-owner old) (emacs-pid)))
      (desktop-release-lock old))
    (desktop-save-mode 1)
    (message "Session %s saved" name)))

(defun taomacs-session-switch (name)
  "Save and close the active session, then restore session NAME.
Closing kills every buffer not matched by `desktop-clear-preserve-buffers',
also when no session is active."
  (interactive (list (taomacs-session--read "Switch to session: " t)))
  (when (equal name (taomacs-session-current))
    (user-error "Session %s is already active" name))
  (when (taomacs-session-current)
    (desktop-save desktop-dirname t))
  (taomacs-session--detach)
  (desktop-clear)
  (desktop-read (taomacs-session--dir name))
  ;; `desktop-read' clears `desktop-dirname' when the session is locked by
  ;; another Emacs and loading it anyway is declined.
  (desktop-save-mode (if desktop-dirname 1 -1)))

(defun taomacs-session-delete (name)
  "Delete the saved session NAME.
Deleting the active session keeps its buffers but stops saving them."
  (interactive (list (taomacs-session--read "Delete session: " t)))
  (unless (yes-or-no-p (format "Delete session %s? " name))
    (user-error "Session %s kept" name))
  (when (equal name (taomacs-session-current))
    (taomacs-session--detach))
  (delete-directory (taomacs-session--dir name) t)
  (message "Session %s deleted" name))

;;;; Eshell

(defun taomacs-session--eshell-save (_desktop-dirname)
  "Return this Eshell buffer's directory as its desktop data."
  default-directory)

(defun taomacs-session--eshell-restore (_file-name buffer-name misc)
  "Restore Eshell buffer BUFFER-NAME in directory MISC.
Skip remote and missing directories, as Ghostel's handler does."
  (if (and (stringp misc) (not (file-remote-p misc)) (file-directory-p misc))
      (let ((default-directory misc))
        (eshell t))
    (message "Desktop: skipping Eshell %s; %s is unavailable" buffer-name misc)
    nil))

(defun taomacs-session-setup-eshell ()
  "Include the current Eshell buffer in saved sessions."
  (setq-local desktop-save-buffer #'taomacs-session--eshell-save))

(add-to-list 'desktop-buffer-mode-handlers
             '(eshell-mode . taomacs-session--eshell-restore))

(provide 'taomacs-session)
;;; taomacs-session.el ends here
