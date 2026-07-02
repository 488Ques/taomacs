;;; taomacs-lang-data.el --- Data/markup languages -*- lexical-binding: t -*-

(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(provide 'taomacs-lang-data)
;;; taomacs-lang-data.el ends here
