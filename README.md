# typescript.el

[![Build Status](https://api.travis-ci.org/ananthakumaran/typescript.el.svg?branch=master)](https://travis-ci.org/ananthakumaran/typescript.el)

`typescript.el` is major-mode for editing [Typescript](http://www.typescriptlang.org/)-files in [GNU Emacs](https://www.gnu.org/software/emacs/).

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

# Other Typescript-packages of interest

While `typescript.el` may *not* provide a full kitchen-sink, the good
news is that there's other packages which do!

More advanced features can be provided by using these additional
packages:

* [tide](https://github.com/ananthakumaran/tide/) - TypeScript
  Interactive Development Environment for Emacs
* [ts-comint](https://github.com/josteink/ts-comint) - a Typescript REPL
  in Emacs.

Initialization these with `typescript.el` will then become a matter of
creating your own `typescript-mode-hook` in your `init.el` file.
