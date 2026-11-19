;;; taomacs-buffers.el --- Prune unused buffers -*- lexical-binding: t -*-

;;; Commentary:

;; Kill buffers that have not been shown for a while.  The rules are those
;; of `clean-buffer-list' from midnight.el, but it runs from an idle timer
;; instead of once a day.  Besides what `clean-buffer-list' already spares
;; (visible buffers, buffers with a process, modified file buffers), pruning
;; never kills pinned buffers, buffers shown in another tab, or internal
;; buffers whose names start with a space.

;;; Code:

(require 'midnight)
(require 'tab-bar)

(defvar-local taomacs-buffer-pinned nil
  "Non-nil means pruning never kills this buffer.")
(put 'taomacs-buffer-pinned 'permanent-local t)

(defun taomacs-buffer-toggle-pin ()
  "Toggle whether pruning may kill the current buffer."
  (interactive)
  (setq taomacs-buffer-pinned (not taomacs-buffer-pinned))
  (force-mode-line-update)
  (message "%s %s" (buffer-name) (if taomacs-buffer-pinned "pinned" "unpinned")))

(defun taomacs-buffer-keep-p (name)
  "Return non-nil if pruning must spare the buffer named NAME.
That is a pinned buffer or one shown in a tab other than the current one."
  (let ((buffer (get-buffer name)))
    (or (buffer-local-value 'taomacs-buffer-pinned buffer)
        (tab-bar-get-buffer-tab buffer t t))))

(defun taomacs-buffer-prune ()
  "Kill buffers that have not been shown recently.
See `clean-buffer-list'.  A buffer that has never been shown starts
ageing the first time this runs.  Logs one summary line."
  (let ((now (current-time))
        (names (mapcar (lambda (buffer) (cons buffer (buffer-name buffer)))
                       (buffer-list))))
    ;; `clean-buffer-list' never kills a buffer without a display time.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (unless buffer-display-time
          (setq buffer-display-time now))))
    ;; It also logs one line per buffer; keep that out of *Messages*.
    (let ((message-log-max nil)
          (inhibit-message t))
      (clean-buffer-list))
    (when-let* ((killed (delq nil (mapcar (lambda (entry)
                                            (unless (buffer-live-p (car entry))
                                              (cdr entry)))
                                          names))))
      (let ((inhibit-message t))
        (message "Pruned %d buffer%s: %s" (length killed)
                 (if (cdr killed) "s" "") (string-join killed ", "))))))

(add-to-list 'clean-buffer-list-kill-regexps "\\`\\*helpful ")
(add-to-list 'clean-buffer-list-kill-never-regexps "\\` ")
(add-to-list 'clean-buffer-list-kill-never-regexps #'taomacs-buffer-keep-p)

(add-to-list 'mode-line-misc-info '(taomacs-buffer-pinned " Pinned "))

;; Sessions keep the pin.
(with-eval-after-load 'desktop
  (add-to-list 'desktop-locals-to-save 'taomacs-buffer-pinned))

(provide 'taomacs-buffers)
;;; taomacs-buffers.el ends here
