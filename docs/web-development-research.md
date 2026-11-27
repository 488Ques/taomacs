# Emacs web tooling: assessment

## Recommendation

Keep the existing stack on Emacs 31.1: built-in Tree-sitter modes (`mhtml-ts-mode`, `js-ts-mode`, `css-ts-mode`, `typescript-ts-mode`/`tsx-ts-mode`), Eglot for TypeScript, Corfu for the completion UI, and Apheleia/oxfmt for TS/TSX formatting. Do not add Tide, `js2-mode`, or Company just to obtain the features already supplied by this stack. Consider a CSS language server only if CSS diagnostics/completions/navigation are wanted; Eglot already has a `css-ts-mode` server mapping, but the checked-in configuration does not currently start Eglot for CSS.

The configuration in [init.el](../init.el) uses built-in Tree-sitter modes, Corfu, Eglot, and Apheleia. A local connection to TypeScript 7 verified the native server and live diagnostics.

## TypeScript 6 and 7

The version-aware contact supports mixed-version projects. A project-local executable `node_modules/.bin/tsc` without `node_modules/typescript/lib/tsserver.js` selects TypeScript 7's native `--lsp --stdio` server. Otherwise Eglot uses `typescript-language-server --stdio` for the tsserver-based installation.

Install `typescript-language-server` separately and make it available on Emacs's PATH for TypeScript 6 and older projects. Selection was checked with real TypeScript 6.0.3 and 7.0.2 installations, and both server commands completed LSP initialization. A real Eglot connection against TypeScript 7.0.2 also reported a type error through Flymake and cleared it after an unsaved correction. Emacs 31.1's bundled Eglot supports the required pull-diagnostics protocol.

The separate TypeScript 7 configuration-registration workaround remains: the native server registers for `workspace/didChangeConfiguration` updates even when the client declines dynamic registration. The configuration forwards changes to `eglot-workspace-configuration` while that registration is active and removes the watcher on unregistration or server shutdown. Eglot sends the initial configuration on connection. This behavior is independent of the version-aware server selection.

Sources: [Eglot maintainer's pull-diagnostics discussion](https://github.com/joaotavora/eglot/discussions/1559#discussioncomment-15246459) (TypeScript native server requires pull diagnostics; Eglot support arrived after Emacs 30.2); [GNU ELPA Eglot releases](https://elpa.gnu.org/packages/eglot.html).

## CSS, JavaScript modes, Tide, completion

- **CSS LSP:** Emacs 30.2's own Eglot server table maps `css-mode` and `css-ts-mode` to `vscode-css-language-server --stdio`, with `css-languageserver --stdio` as an alternative. The upstream `vscode-langservers-extracted` package supplies the `vscode-css-language-server` executable and extracts CSS/HTML/JSON/ESLint servers from VS Code. That is a reasonable optional CSS server; it is not necessary for Tree-sitter syntax highlighting/indentation, and this setup currently has no `css-ts-mode` Eglot hook. Add one only if LSP features are desired.
- **`js2-mode` vs `js-ts-mode`:** `js-ts-mode` is already the chosen built-in mode here and is the maintainable default for Emacs 30.2. `js2-mode` is an alternate JavaScript editing mode; its upstream README specifically recommends built-in `js-mode` plus `js2-minor-mode` when its JS2 AST integrations are wanted. That is a specialized reason to add it, not a replacement needed alongside the current Tree-sitter mode.
- **Tide:** Tide is an Emacs client for `tsserver` and its README's standard setup enables Company, Flycheck, Eldoc, Tide highlighting, and Tide formatting. Eglot already owns the TypeScript LSP connection in this configuration; adding Tide would introduce overlapping language-service features and another server/client configuration. Also, the TypeScript 7 `tsserver` compatibility concern applies to Tide's legacy-server model.
- **Company vs Corfu:** They are alternative in-buffer completion UIs, not competing language servers. Eglot supplies completion through Emacs completion-at-point; Corfu presents those candidates in a popup and explicitly relies on CAPFs rather than supplying its own backend. Company can also present completion candidates, but Tide's use of Company is not a reason to install Tide or replace the already-configured Corfu. Keep Corfu.

Sources: [Emacs 30.2 Eglot server mappings](https://github.com/emacs-mirror/emacs/blob/emacs-30.2/lisp/progmodes/eglot.el) (`eglot-server-programs`, JS/TS and CSS entries); [`vscode-langservers-extracted` upstream README](https://github.com/hrsh7th/vscode-langservers-extracted); [`js2-mode` upstream README](https://github.com/mooz/js2-mode#react-and-jsx); [Tide upstream README](https://github.com/ananthakumaran/tide); [Corfu upstream README](https://github.com/minad/corfu); [Eglot manual, features](https://www.gnu.org/software/emacs/manual/html_node/eglot/Eglot-Features.html).
