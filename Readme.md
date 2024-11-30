-----------
[![License GPL 3][badge-license]](http://www.gnu.org/licenses/gpl-3.0.txt)


# Usage

- load file metta-mode.el

``` elisp
(add-to-list 'load-path
             ;; fix path
             "metta-mode-alpha/")
(require 'metta-mode)

```


# Interactive Developement

Right now you have to manually edit metta-run-inferior-metta, 

- run `metta-run-inferior-metta`

- `metta-eval-string` is roughly the same as typing in a metta repl


- to eval and display results, https://github.com/abo-abo/lispy automatically works. 
- TODO: 'eval-last-sexp' and so forth 



# Lispy



## License

MeTTa Mode is distributed under the GNU General Public License, version 3.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
