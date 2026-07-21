;;; taomacs-ai.el --- AI agents -*- lexical-binding: t -*-

;; agent-shell: native Emacs shell for LLM agents over ACP.
;; Claude uses the existing Claude Code login (no API key stored here).
;; Requires the `claude-agent-acp' adapter on PATH; install once with:
;;   mise use -g "npm:@agentclientprotocol/claude-agent-acp"
(use-package agent-shell
  :ensure t
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t))
  (agent-shell-session-restore-verbosity 'full)
  :bind
  (("C-c a" . agent-shell-anthropic-start-claude-code)))

(provide 'taomacs-ai)
;;; taomacs-ai.el ends here
