# .emacs.d

My personal Emacs config.

Runs on **Emacs 30** (macOS / Apple Silicon). Tuned for PHP, C/C++, Go and web
development, with LSP, fuzzy navigation, Git, and a Claude Code integration.

## Highlights

- **Completion & checking** — `company` + `flycheck`
- **Navigation** — `helm`, `projectile`, `helm-gtags`
- **Git** — `magit`
- **UI** — `doom-modeline`, `grandshell` theme, transparent in the terminal
- **C/C++ LSP** — built-in `eglot` + `clangd`, with format-on-save
- **Claude Code** — run [Claude Code](https://github.com/anthropics/claude-code)
  in a side window via `claude-code.el` + `vterm`

## Requirements

Emacs 30+ and a few CLI tools (Homebrew):

```sh
brew install emacs aspell w3m pkg-config cmake
# clangd ships with Xcode Command Line Tools (xcode-select --install)
```

Most Emacs packages auto-install on first launch (`use-package` / `package.el`).

## Install

```sh
git clone git@github.com:xuwenzhi/.emacs.d.git ~/.emacs.d
emacs   # first start installs packages
```

## Handy keys

| Key | Action |
| --- | --- |
| `M-<arrow>` | move between windows |
| `M-;` / `C-x C-;` | comment / uncomment |
| `C-x g` | Magit status |
| `M-.` / `M-,` | jump to definition / back (LSP in C/C++) |
| `C-c C-c c` | open Claude Code |
| `C-c C-c C` | resume a Claude Code session |
| `C-c C-c s` | send a prompt to Claude Code |
| `C-c C-c x` | send region + context to Claude Code |
| `C-c C-c k` | kill the Claude Code session |
