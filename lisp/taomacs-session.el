;;; taomacs-session.el --- Named desktop.el sessions -*- lexical-binding: t -*-

;;; Commentary:

;; A session is a named desktop saved in its own directory under
;; `taomacs-session-directory'.  At most one session is active; while one
;; is, `desktop-save-mode' saves it when idle, on switching, and on exit.
;; Switching saves the active session, kills its buffers (`desktop-clear'),
;; and restores the other session's buffers, frames, tabs, and windows.
;; Ghostel restores its terminals through its own desktop handler; this
;; file adds handlers for Eshell and Magit status buffers.

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

(defun taomacs-session--checkpoint (dir)
  "Return the checkpoint file of the session in DIR.
It is a copy of the desktop file taken by `taomacs-session-save', which
auto-saves never touch."
  (concat (desktop-full-file-name dir) ".checkpoint"))

(defun taomacs-session--restore (dir)
  "Load the session in DIR and auto-save it if that succeeded."
  (desktop-read dir)
  ;; `desktop-read' clears `desktop-dirname' when the session is locked by
  ;; another Emacs and loading it anyway is declined.
  (desktop-save-mode (if desktop-dirname 1 -1)))

(defun taomacs-session-save (name)
  "Save frames, tabs, windows, and buffers as session NAME and activate it.
Also record the saved state as the checkpoint `taomacs-session-revert'
restores.  Interactively, save the active session; with a prefix
argument, or when no session is active, read NAME."
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
    (copy-file (desktop-full-file-name dir) (taomacs-session--checkpoint dir) t)
    (when (and old (not (equal old dir)) (eql (desktop-owner old) (emacs-pid)))
      (desktop-release-lock old))
    (desktop-save-mode 1)
    (message "Session %s saved" name)))

(defun taomacs-session--leave ()
  "Save and detach the active session, then kill its buffers.
Kills every buffer not matched by `desktop-clear-preserve-buffers',
also when no session is active."
  (when (taomacs-session-current)
    (desktop-save desktop-dirname t))
  (taomacs-session--detach)
  (desktop-clear))

(defun taomacs-session-switch (name)
  "Save and close the active session, then restore session NAME.
See `taomacs-session--leave' for which buffers closing kills."
  (interactive (list (taomacs-session--read "Switch to session: " t)))
  (when (equal name (taomacs-session-current))
    (user-error "Session %s is already active" name))
  (taomacs-session--leave)
  (taomacs-session--restore (taomacs-session--dir name)))

(defun taomacs-session-revert ()
  "Restore the active session to its last `taomacs-session-save'.
Kill its buffers without saving them and discard every change since,
including later auto-saves."
  (interactive)
  (let* ((name (or (taomacs-session-current) (user-error "No active session")))
         (dir desktop-dirname)
         (checkpoint (taomacs-session--checkpoint dir)))
    (unless (file-exists-p checkpoint)
      (user-error "No checkpoint for session %s; save it first" name))
    (unless (yes-or-no-p (format "Revert session %s to its last manual save? " name))
      (user-error "Session %s kept" name))
    (taomacs-session--detach)
    (desktop-clear)
    (copy-file checkpoint (desktop-full-file-name dir) t)
    (taomacs-session--restore dir)
    (message "Session %s reverted" name)))

(defun taomacs-session-close ()
  "Save and close the active session, leaving one frame and tab on *scratch*.
See `taomacs-session--leave' for which buffers closing kills."
  (interactive)
  (let ((name (taomacs-session-current)))
    (taomacs-session--leave)
    (delete-other-frames)
    (tab-bar-close-other-tabs)
    (tab-bar-rename-tab "")             ; back to the automatic name
    (switch-to-buffer (get-scratch-buffer-create))
    (message (if name (format "Session %s closed" name) "Buffers closed"))))

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

;;;; Frames

(defun taomacs-session--filter-position (current filtered parameters saving)
  "Drop `left' and `top' of maximized frames; otherwise filter as frameset does.
A maximized macOS frame can report its position from before it was
maximized, and `frameset-restore' applies the saved position even to an
already-maximized frame, moving it off its place.  Filtering on restore
too covers sessions saved before this filter existed.  For CURRENT,
FILTERED, PARAMETERS and SAVING see `frameset-filter-alist'."
  (and (not (memq (cdr (assq 'fullscreen parameters)) '(maximized fullboth)))
       (frameset-filter-shelve-param current filtered parameters saving)))

(dolist (param '(left top))
  (push (cons param #'taomacs-session--filter-position) frameset-filter-alist))

;;;; Eshell and Magit status

(defun taomacs-session--save-directory (_desktop-dirname)
  "Return this buffer's directory as its desktop data."
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
  (setq-local desktop-save-buffer #'taomacs-session--save-directory))

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

(defun taomacs-session--magit-restore (_file-name buffer-name misc)
  "Restore Magit status buffer BUFFER-NAME for the repository in MISC.
Leave windows alone: the session's layout is restored afterwards."
  (if (and (stringp misc) (not (file-remote-p misc)) (file-directory-p misc))
      (save-window-excursion
        (magit-status-setup-buffer misc))
    (message "Desktop: skipping %s; %s is unavailable" buffer-name misc)
    nil))

(defun taomacs-session-setup-magit ()
  "Include the current Magit status buffer in saved sessions."
  (setq-local desktop-save-buffer #'taomacs-session--save-directory))

(add-to-list 'desktop-buffer-mode-handlers
             '(eshell-mode . taomacs-session--eshell-restore))
(add-to-list 'desktop-buffer-mode-handlers
             '(magit-status-mode . taomacs-session--magit-restore))

(provide 'taomacs-session)
;;; taomacs-session.el ends here
