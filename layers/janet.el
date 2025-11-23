(setq treesit-language-source-alist
      (if (eq 'windows-nt system-type)
          '((janet-simple
             . ("https://github.com/sogaiu/tree-sitter-janet-simple"
                nil nil "gcc.exe")))
        '((janet-simple
           . ("https://github.com/sogaiu/tree-sitter-janet-simple")))))

(when (not (treesit-language-available-p 'janet-simple))
  (treesit-install-language-grammar 'janet-simple))

(straight-use-package
 '(janet-ts-mode :host github
                 :repo "sogaiu/janet-ts-mode"
                 :files ("*.el")))

(use-package janet-ts-mode
  :straight t)

(straight-use-package
 '(ajrepl :host github
          :repo "sogaiu/ajrepl"
          :files ("*.el" "ajrepl")))

(use-package ajrepl
  :straight t
  :config
  (add-hook 'janet-ts-mode-hook
            #'ajrepl-interaction-mode))

(provide 'janet)
