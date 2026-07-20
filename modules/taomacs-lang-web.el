;;; taomacs-lang-web.el --- TypeScript/React/Astro support -*- lexical-binding: t -*-

;; Auto-start eglot for TS/TSX.  eglot ships the typescript-ts-mode and
;; tsx-ts-mode -> "typescript-language-server --stdio" mappings.
;; The .ts/.tsx -> *-ts-mode routing lives in taomacs-dev.el.
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook        #'eglot-ensure)

;; Format-on-save with the project-local oxfmt (Oxc formatter).  `npx' makes
;; Apheleia prefer node_modules/.bin/oxfmt; `filepath' lets oxfmt infer the
;; parser from the real file name.  oxfmt reads stdin, writes stdout.
(with-eval-after-load 'apheleia
  (setf (alist-get 'oxfmt apheleia-formatters)
        '(npx "oxfmt" "--stdin-filepath" filepath))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'oxfmt)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'oxfmt))

;; Astro (light): clean highlighting of the frontmatter + HTML + JSX mix.
;; No language server, no formatter---these files are edited rarely.
(use-package web-mode
  :ensure t
  :mode "\\.astro\\'")

(provide 'taomacs-lang-web)
;;; taomacs-lang-web.el ends here
