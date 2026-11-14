;;; early-init.el --- Early startup settings -*- lexical-binding: t; -*-

;; Startup speed
(setq gc-cons-threshold 10000000)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(setq default-frame-alist '((fullscreen . maximized)
                            (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)))
