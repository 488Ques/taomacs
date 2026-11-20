;;; taomacs-mcp.el --- Expose this Emacs to agents over MCP -*- lexical-binding: t -*-

;;; Commentary:

;; agent-shell hands each new omp session an "emacs" MCP server: `nc -U'
;; bridging omp's stdio to this Emacs's mcp-server socket.  The socket is
;; named per process, so each Emacs instance serves only its own shells.

;;; Code:

(require 'mcp-server)

(defun taomacs-mcp-socket ()
  "Start this Emacs's MCP server if needed and return its socket path."
  (unless mcp-server-running
    (mcp-server-start-unix)
    ;; Remove the socket on exit; per-process names would otherwise pile up.
    (add-hook 'kill-emacs-hook #'taomacs-mcp-stop))
  (mcp-server-transport-unix-socket-path))

(defun taomacs-mcp-stop ()
  "Stop the MCP server if it is running, removing its socket."
  (when mcp-server-running
    (mcp-server-stop)))

(provide 'taomacs-mcp)
;;; taomacs-mcp.el ends here
