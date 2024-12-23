-----------
[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

MeTTa
A language for cognitive computations

OpenCog Hyperon, towards AGI

https://metta-lang.dev/

![showcase gif](https://i.imgur.com/jMhJ0ip.gif)

# Usage

- load file metta-mode.el

example:

``` elisp
;; fix your path

(add-to-list 'load-path "/home/benj/repos/metta-mode-alpha/")

(when (require 'metta-mode nil t)
  (set-face-attribute
   'metta-operators-face
   nil
   :bold t
   :foreground nil
   :box nil))
```

# Interactive Developement

> ⚠ I did not implement any interactive without a dependency on [lispy](https://github.com/abo-abo/lispy) at the moment.

- redifine `metta-start-metta-repl` to your needs.
- run `metta-run-inferior-metta`
- `metta-eval-string` is roughly the same as typing in a metta repl

- TODO: 'eval-last-sexp' and so forth
- TODO: consider using the scheme geiser interface


# Features

- tiny fontifation
- tiny completion via 'completion-at-point'

- TODO: completion with lsp
- TODO: completion with inferior metta?
- TODO: fontification for function symbols, type symbols etc.?

- TODO: merge stuff from https://github.com/trueagi-io/metta-wam/blob/master/libraries/lsp_server_metta/lsp-metta.el


# Acknowledgments

I copied stuff from https://github.com/Amanuel-1/metta-lang-highlighter for syntax highlighting.


## License

MeTTa Mode is distributed under the GNU General Public License, version 3.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
