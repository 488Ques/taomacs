# ultra-scroll setup

## Minimal setup

Upstream's README-supported `use-package` declaration is (the optional `:vc` line is for Emacs 30+):

```elisp
(use-package ultra-scroll
  ;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3) ; or whatever value you prefer, since v0.4
        ;scroll-margin 0)        ; scroll-margin>0 is supported since v0.7
  :config
  (ultra-scroll-mode 1))
```

Source: [upstream README, Installation](https://github.com/jdtsmith/ultra-scroll#installation). The package is published on [MELPA](https://melpa.org/#/ultra-scroll) and [MELPA Stable](https://stable.melpa.org/#/ultra-scroll). This config already adds `https://melpa.org/packages/` to `package-archives` in `init.el`, so package.el has the upstream-listed archive available.

## Interaction and caveats

Replace the standalone `(pixel-scroll-precision-mode)` invocation with the declaration above; do not independently turn on both. Upstream states that `ultra-scroll-mode` itself enables `pixel-scroll-precision-mode` and remaps its scroll command. If toggling modes in one session, disable `ultra-scroll-mode` before re-enabling `pixel-scroll-precision-mode`. Optional interpolated page scrolling can continue to use `pixel-scroll-precision-interpolate-page`. [Upstream interoperability docs](https://github.com/jdtsmith/ultra-scroll#pixel-scroll-precision-comparison-and-interoperability); implementation: [`ultra-scroll-mode`](https://github.com/jdtsmith/ultra-scroll/blob/master/ultra-scroll.el).

`ultra-scroll` is intended for hardware/system combinations providing genuine variable pixel scroll data, not interpolated mouse events. Use `M-x ultra-scroll-check` to inspect support. On systems without usable pixel events, built-in `pixel-scroll-precision-mode` with `pixel-scroll-precision-interpolate-mice` is the upstream-suggested alternative. [Compatibility](https://github.com/jdtsmith/ultra-scroll#compatibility), [comparison](https://github.com/jdtsmith/ultra-scroll#pixel-scroll-precision-comparison-and-interoperability).

On macOS, upstream lists standard Emacs builds and emacs-mac as supported; OS/driver-provided momentum works independently of Emacs's momentum setting. emacs-mac-specific swipe-to-scroll and pinch gestures are exclusive to that port. [README overview](https://github.com/jdtsmith/ultra-scroll#ultra-scroll-scroll-emacs-like-lightning-), [interoperability](https://github.com/jdtsmith/ultra-scroll#pixel-scroll-precision-comparison-and-interoperability). In terminal/TTY use, smooth behavior depends on whether the system/input delivers pixel-level scroll events; the README makes hardware event data, not GUI status, the compatibility condition. Check actual input with `ultra-scroll-check`; no separate terminal setup is documented. On Linux/X11, upstream code warns when Emacs lacks XInput2; Linux pixel-wheel support may also vary by toolkit/input manager. [README compatibility](https://github.com/jdtsmith/ultra-scroll#compatibility), [mode implementation](https://github.com/jdtsmith/ultra-scroll/blob/master/ultra-scroll.el).

## Sources

- [Upstream README](https://github.com/jdtsmith/ultra-scroll)
- [Upstream implementation](https://github.com/jdtsmith/ultra-scroll/blob/master/ultra-scroll.el)
- [MELPA package listing](https://melpa.org/#/ultra-scroll)
- [MELPA Stable package listing](https://stable.melpa.org/#/ultra-scroll)
