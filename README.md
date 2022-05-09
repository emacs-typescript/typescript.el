# typescript.el

![Build & Test](https://github.com/emacs-typescript/typescript.el/workflows/Build%20&%20Test/badge.svg)
[![MELPA](https://melpa.org/packages/typescript-mode-badge.svg)](https://melpa.org/#/typescript-mode)
[![MELPA Stable](https://stable.melpa.org/packages/typescript-mode-badge.svg)](https://stable.melpa.org/#/typescript-mode)

`typescript.el` is a major-mode for editing [Typescript](http://www.typescriptlang.org/)-files in [GNU Emacs](https://www.gnu.org/software/emacs/).

`typescript.el` is a self-contained, lightweight and minimalist major-mode
focused on providing basic font-lock/syntax-highlighting and
indentation for Typescript syntax, without any external dependencies.

Output from `tsc` and `tslint` is also handled seamlessly through
`compilation-mode`.

# Installation

`typescript.el` can be installed from source directly using your
favourite approach or framework, or from MELPA and MELPA Stable as a
package.

To install typescript.el simply type `M-x package-install<RET>typescript-mode<RET>`.

# Customization

To customize `typescript.el` just type the following: `M-x customize-group<RET>typescript<RET>`.

You can add any other customization you like to `typescript-mode-hook`
in your `init.el` file. `typescript.el` also handles `prog-mode-hook`
on versions of Emacs which supports it.

# Support for Compilation Mode

This mode automatically adds support for `compilation-mode` so that if
you run `M-x compile<ret>tsc<ret>` the error messages are correctly
parsed.

However, the error messages produced by `tsc` when its `pretty` flag
is turned on include ANSI color escapes, which by default
`compilation-mode` does not interpret. In order to get the escapes
parsed, you can use:

```elisp
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
```

Or, if you prefer, you can configure `tsc` with the `pretty` flag set
to `false`: `tsc --pretty false`. However, doing this does more than
just turning off the colors. It also causes `tsc` to produce less
elaborate error messages.

# Contributing

To run the tests you can run `make test`.

If you prefer, you may run the tests via the provided `Dockerfile`.

```bash
docker build -t typescript-mode .
docker run --rm -v $(pwd):/typescript-mode typescript-mode
```

# Other Typescript-packages of interest

While `typescript.el` may *not* provide a full kitchen-sink, the good
news is that there's other packages which do!

More advanced features can be provided by using these additional
packages:

* [lsp-mode](https://github.com/emacs-lsp/lsp-mode) - A standards-based
  code-completion and refactoring backend, based on the
  [Language Server Protocol (LSP)](https://langserver.org/).

* [tide](https://github.com/ananthakumaran/tide/) - TypeScript
  Interactive Development Environment for Emacs
* [ts-comint](https://github.com/josteink/ts-comint) - a Typescript REPL
  in Emacs.

Initializing these with `typescript.el` will then become a matter of
creating your own `typescript-mode-hook` in your `init.el` file.


# Tree sitter integration
For now we have integration with both tree-sitter implementations. The oldest
one, the rust variant by @ubolonton is defined in `typescript-tree-sitter.el`,
and the newer, emacs feature branch variant is defined in
`typescript-ts.el`. The former requires the tree-sitter packages from MELPA, and
the latter requires an emacs built from the feature/tree-sitter upstream branch.

Steps to get things working for now:

1. Get the tree sitter lib dependency for emacs:
```
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter
make
(sudo) make install
```
2. Get emacs from the feature branch:
- `git clone -b feature/tree-sitter git://git.sv.gnu.org/emacs.git`
- `cd emacs && make bootstrap && (sudo) make install`

3. From the typescript repo
- run `install-tsx.sh `. If this script doesn't work for you try to find some
  guide on the interwebs and report back to me.
- move the compiled file from `dist/` to `~/emacs/tree-sitter`

4. enable `typescript-ts-mode` in some `.tsx` or `.ts` file and off you go

