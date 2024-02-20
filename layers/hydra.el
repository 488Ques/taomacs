;; Emacs binding that sticks around
(use-package hydra
  :bind
  (("C-c r" . hydra-resize-window/body))
  :config
  (defhydra hydra-resize-window ()
    "Resize window"
    ("h" #'shrink-window-horizontally)
    ("l" #'enlarge-window-horizontally)
    ("j" #'shrink-window)
    ("k" #'enlarge-window)
    ("q" nil)))
