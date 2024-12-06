
-----------
[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)

MeTTa
A language for cognitive computations

OpenCog Hyperon, towards AGI

https://metta-lang.dev/

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

- redifine `metta-start-metta-repl` to your needs. 
- run `metta-run-inferior-metta`
- `metta-eval-string` is roughly the same as typing in a metta repl

- to eval and display results, https://github.com/abo-abo/lispy automatically works. 
- TODO: 'eval-last-sexp' and so forth 

# Features

- tiny fontifation
- tiny completion via 'completion-at-point'

- TODO: completion with lsp
- TODO: completion with inferior metta? 
- TODO: fontification for function symbols, type symbols etc.? 


# Acknowledgments

I copied stuff from https://github.com/Amanuel-1/metta-lang-highlighter for syntax highlighting.


## License

MeTTa Mode is distributed under the GNU General Public License, version 3.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
