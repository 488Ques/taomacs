(use-package yasnippet
  :init
  ;; Turn on yas after emacs starts up
  (yas-global-mode))

(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.css\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :config
  (setq web-mode-markup-indent-offser 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2))

(use-package emmet-mode
  :hook (web-mode . emmet-mode)
  :config
  (setq emmet-indent-after-insert nil
	emmet-indentation 2))

(provide 'web)
