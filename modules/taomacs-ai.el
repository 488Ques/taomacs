;;; taomacs-ai.el --- AI agents -*- lexical-binding: t -*-

;; agent-shell: native Emacs shell for LLM agents over ACP.
;; Driven by oh-my-pi, which speaks ACP through `omp acp'.  The binary
;; comes from `bun install -g @oh-my-pi/pi-coding-agent' and picks up its
;; own auth/config from ~/.omp, so no keys are stored here.
(use-package agent-shell
  :ensure t
  :custom
  ;; Skip the agent picker -- always talk to omp.
  (agent-shell-preferred-agent-config 'omp)
  (agent-shell-session-restore-verbosity 'full)
  :bind
  (("C-c A" . agent-shell)))

(provide 'taomacs-ai)
;;; taomacs-ai.el ends here
