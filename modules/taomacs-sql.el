;;; taomacs-sql.el --- SQL client and database tooling -*- lexical-binding: t -*-

;; A lightweight SQL workbench: connection management, interactive REPLs,
;; and dabbrev-based completion for PostgreSQL, MySQL/MariaDB, and SQLite,
;; built on the built-in `sql.el'.

;;; Connections
;;
;; Define your databases in `local.el' (git-ignored) via
;; `taomacs-sql-connections'.  Passwords are NOT stored here -- put them in
;; ~/.authinfo.gpg.  Example `local.el' entry:
;;
;; (setq taomacs-sql-connections
;;       '((prod-pg    (sql-product 'postgres) (sql-server "db.example.com")
;;                     (sql-port 5432) (sql-user "me") (sql-database "app"))
;;         (local-mysql (sql-product 'mysql) (sql-server "127.0.0.1")
;;                      (sql-port 3306) (sql-user "root") (sql-database "app"))
;;         (local-lite  (sql-product 'sqlite) (sql-database "~/data/app.db"))))
;;
;; Example ~/.authinfo.gpg line:
;;   machine db.example.com port 5432 login me password s3cret

;;; Code:

(defvar taomacs-sql-connections nil
  "Database connections in `sql-connection-alist' format.
Set this in `local.el'.  Each entry is (NAME (VAR VALUE) ...), e.g.
 (mydb (sql-product 'postgres) (sql-server \"host\") (sql-user \"me\")
       (sql-database \"db\")).")

(defun taomacs-sql-connect ()
  "Refresh `sql-connection-alist' from `taomacs-sql-connections' and connect.
Reading connections at call time (not module load) keeps them correct
regardless of when `local.el' is loaded relative to this module."
  (interactive)
  (require 'sql)
  (setq sql-connection-alist taomacs-sql-connections)
  (call-interactively #'sql-connect))

;; Built-in `sql.el' -- REPL, send-region, object listing.  No :ensure.
(use-package sql
  :bind (("C-c d c" . taomacs-sql-connect)      ; connect to a named database
         ("C-c d b" . sql-show-sqli-buffer)     ; show/switch the SQLi REPL
         ("C-c d l" . sql-list-all)             ; dump schema names (feeds dabbrev)
         ("C-c d s" . sqlite-mode-open-file)))  ; browse a local SQLite file

;; Pure-Elisp SQL indentation.
(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

;; ---------------------------------------------------------------------------
;; UPGRADE PATH -- schema-aware completion via the `sqls' LSP server.
;;
;; Completion today is dabbrev (see `cape-dabbrev' in taomacs-completion.el):
;; after connecting, run `C-c d l' (`sql-list-all') to dump object names into
;; the SQLi buffer; dabbrev then completes them in your .sql buffer.  Re-run
;; after schema changes.
;;
;; To upgrade to structured, scoped, auto-updating completion, install nothing
;; (the `sqls' binary is already on PATH via mise) and enable eglot for SQL.
;; VALIDATE the eglot<->sqls config handshake before relying on it; the
;; fallback is generating ~/.config/sqls/config.yml from
;; `taomacs-sql-connections'.
;;
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '(sql-mode . ("sqls")))
;;   (add-hook 'sql-mode-hook #'eglot-ensure))
;; ---------------------------------------------------------------------------

(provide 'taomacs-sql)
;;; taomacs-sql.el ends here
