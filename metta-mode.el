;;; metta-mode.el --- MeTTa Interactive Development Environment, Towards Magic -*- lexical-binding: t; -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.


;;
;; Author: Benjamin Schwerdtner <Benjamin.Schwerdtner@gmail.com>
;; Author: Douglas R. Miles,
;;         https://github.com/trueagi-io/metta-wam/blob/master/libraries/lsp_server_metta/lsp-metta.el
;; 
;; URL:
;; Version: 0.01
;; Keywords: lsp, metta
;;  


;;; Commentary:
;;
;; This Emacs package provides a Metta language major mode and integrates
;; an LSP client for enhanced language features. To use this package,
;; simply place it in your Emacs directory, typically ~/.emacs.d/,
;; and add the following line to your ~/.emacs or init.el file:
;;
;; (load "path/to/lsp-metta.el")
;;
;; Ensure you adjust "path/to/" to the actual path where you saved this file.
;; This will set up the major mode and LSP client whenever you open a Metta
;; file with the .metta extension.
;;

;; Example using use-package:
;;
;; (use-package metta-mode
;;   :load-path "path/to/lsp-metta"  ; Adjust the path as needed
;;   :config
;;   (setq some-metta-config-var 'value)
;;   (add-hook 'metta-mode-hook #'lsp))
;;
;;; Code:


;; --------

;; Define the mode's keymap
(defvar metta-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Define keybindings here if needed
    map)
  "Keymap for `metta-mode'.")

;; ------------------------------------------

;; 
;; - merged 
;; https://github.com/trueagi-io/metta-wam/blob/master/libraries/lsp_server_metta/lsp-metta.el
;; Author: Douglas R. Miles
;; With small deviations, like 'cdr-atom' has font lock builtin face instead of keyword face.
;; 
;; 


;; 
;; copied from clojure-mode
;; https://github.com/clojure-emacs/clojure-mode
;;
;;
;; clojure-mode contains the following License notice:i
;; 
;;; License:
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Note: 
;; Some of these might not be correct yet, (because it's copied from clojure-mode). 
(defvar metta-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    ;; Control characters from 0-31 default to the punctuation syntax class
    (modify-syntax-entry '(32 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)
    (modify-syntax-entry ?\r " " table)
    ;; Setting commas as whitespace makes functions like `delete-trailing-whitespace' behave unexpectedly (#561)
    (modify-syntax-entry ?, "." table)
    
    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    ;; (modify-syntax-entry ?\{ "(}" table)
    ;; (modify-syntax-entry ?\} "){" table)

    ;; Prefix chars

    (modify-syntax-entry ?! "'" table)

    ;; (modify-syntax-entry ?` "'" table)
    ;; (modify-syntax-entry ?~ "'" table)
    ;; (modify-syntax-entry ?^ "'" table)
    ;; (modify-syntax-entry ?@ "'" table)
    ;; (modify-syntax-entry ?? "_ p" table)
    ;; (modify-syntax-entry ?# "_ p" table)
    (modify-syntax-entry ?' "_ p" table) ; ' is allowed anywhere but the start of symbols

    ;; Others
    (modify-syntax-entry ?\; "<" table)  ; comment start
    (modify-syntax-entry ?\n ">" table)  ; comment end
    (modify-syntax-entry ?\' "\"" table) ;; single quoted string
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table)
  "Syntax table for `metta-mode'.")

(defconst
  metta-operators
  '(">"
    ">="
    "<"
    "<="
    "+"
    "-"
    "*"
    "/"
    "%"
    "=="
    "!="
    "&&"
    ;; I liked the = being green on my setup
    "="
    ))

(defconst metta-grounded-symbols
  '(;; https://github.com/Amanuel-1/metta-lang-highlighter/blob/312ee852c01cdecec8e8243ac51dba384367f023/syntaxes/metta.tmLanguage.json#L61
    "import!"
    "bind!"
    "new-space"
    "add-atom"
    "remove-atom"
    "pragma!"
    "get-type"
    "get-metatype"
    "println!"
    "trace!"
    "nop"
    "new-state"
    "get-state"
    "change-state!"
    "match"
    "car-atom"
    "cdr-atom"
    "cons-atom"
    "assertEqual"
    "assertEqualToResult"
    "collapse"
    "superpose"
    "get-metatype"
    "load-ascii"
    "call"
    "regex"
    "unify"
    "quote"
    "add-reduct"
    "mod-space!"))

(defconst metta-constants
  '("True" "False" "&self" "Type"))

(defface metta-operators-face
  '((t (:inherit font-lock-function-name-face :foreground "red")))
  "Face for operators in Metta mode.")

(defconst
  metta-operators-other
  '("and"
    "or"
    "not"
    "xor"
    "flip"
    "empty"
    "if"
    "case"
    "let"
    "let*"))

(defconst
  metta-mode-font-lock-keywords
  (eval-when-compile
    (let* (;; Regex patterns to match any keyword starting with '@' or '&'
           (at-constants "@\\w+")
           (amp-constants "&\\w+"))
      `(;; Apply constant face to @ prefixed words
        (,at-constants . font-lock-constant-face)
        ;; Apply constant face to & prefixed words
        (,amp-constants . font-lock-constant-face)
        ;; in
        ;; (= (foo) (bar))
        ;; fontify = ?
        ;; fontify foo ?
        ;;
        ;; -----------------------------
        ;; - type declarations
        ;; - type syntax
        ;; -----------------------------
        (,(concat
           "\\<"
           (regexp-opt '("&self") t)
           "\\>")
         0
         font-lock-type-face)
        ;; special forms
        (
         ,(regexp-opt metta-grounded-symbols 'words)
         1
         font-lock-builtin-face)
        (,(concat "\\<" "\!" "\\>")
         0
         font-lock-keyword-face)
        (,(concat
           "\\<"
           (regexp-opt metta-operators t)
           "\\>")
         1
         'metta-operators-face)
        (,(concat
           "("
           (regexp-opt
            metta-operators-other
            t)
           "\\>")
         1
         font-lock-keyword-face)
        (,(regexp-opt
           metta-constants
           'words)
         0
         font-lock-constant-face)
        ;; ---------------------------
        (,(concat "\\<$" "\\w+" "\\>")
         (0
          font-lock-variable-name-face
          nil
          t))
        (,(concat
           "\\<"
           (regexp-opt '("->" ":"))
           "\\>")
         (0 font-lock-type-face nil t))
        ;; type syntax
        ;; (,(concat
        ;;           "("
        ;;           "\\(->.+\\)"
        ;;           ")")
        ;;  (1
        ;;   font-lock-type-face
        ;;   nil
        ;;   t))
        ))))


(defun metta-font-lock-setup ()
  "Configures font-lock for editing Metta code."
  ;; (setq-local font-lock-multiline t)
  ;; (add-to-list 'font-lock-extend-region-functions
  ;;              #'clojure-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(metta-mode-font-lock-keywords
          nil
          nil
          (("+-*/.<>=!?$%_&:" . "w"))   ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          ;; (font-lock-syntactic-face-function
          ;;  . clojure-font-lock-syntactic-face-function)
          )))

(defun metta-mode-completion-at-point ()
  "Completion source for `completion-at-point-functions'."
  (when-let*
      ((bounds
        (bounds-of-thing-at-point
         'symbol)))
    (list
     (car bounds)
     (cdr bounds)
     (completion-table-dynamic
      (lambda (_)
        (append
         metta-grounded-symbols
         metta-constants
         metta
         '(
           ;; honary
           "Nil"
           "nil"
           "%Undefined%"
           ;; ------------------
           ;; python
           "py-atom"
           "py-dot"
           "py-list"
           "py-dict"
           "py-tuple")
         metta-operators
         metta-operators-other))))))

(define-derived-mode metta-mode prog-mode "MeTTa"
  (metta-font-lock-setup)
  (add-hook 'completion-at-point-functions
            #'metta-mode-completion-at-point
            nil
            t))

;;;###autoload
(progn
  (add-to-list
   'auto-mode-alist
   '("\\.metta\\'" . metta-mode)))



;; -------------------
;; metta-repl
;; ----------------


;; 
;; how to start a metta-repl
;; This currently needs to be customized by the user. !



(defun metta-start-metta-repl ()
  (let ((default-directory "/home/benj/repos/hyperon-experimental/"))
    (start-process
     "MeTTA REPL"
     (get-buffer-create
      "*metta-repl*")
     "cargo"
     "run"
     "--bin"
     "metta-repl")))

(defvar-local metta--request nil)

(defun metta-run-inferior-metta ()
  ;; todo:
  ;; - make it a repl buffer so you can also type
  ;; - run with some start script
  ;; - implement req->response on metta side
  ;; - make it buffer for large outputs
  ;; - be inspried by clojure nrepl
  (interactive)
  (let ((p (metta-start-metta-repl)))
    (set-process-filter
     p
     #'metta-connetion-process-filter)
    (display-buffer
     (process-buffer p))))

(defun metta-connetion-process-filter (proc string)
  (with-current-buffer
      (process-buffer proc)
    (insert string)
    (when-let ((k (plist-get metta--request :k)))
      ;; kludge: filter the result list of something
      ;; this way I differentiate between printing and returning
      ;; results
      ;;
      ;; TODO: 1. request response concept
      ;;
      (let ((string (s-trim string)))
        (when
            (or
             (s-matches? "^\\[.+?\\]$" string)
             (equal ">" string)
             (equal "[]" string))
          (funcall k string)
          (setf metta--request '()))))))

(defun metta-eval (connection string continuation)
  (let* ((orig-buffer (current-buffer))
         (request-id (concat
                      "metta-r-"
                      (current-time)))
         (kont (lambda (output)
                 (with-current-buffer
                     orig-buffer
                   (funcall continuation output)))))
    (with-current-buffer
        connection
      (process-send-string
       (get-buffer-process connection)
       string)
      (process-send-string
       (get-buffer-process connection)
       "\n")
      ;; - the request id could come from the metta side
      ;; - alternative: we evaluate a form roughly like: (request-id . (form))
      (setq-local
       metta--request
       `(:input ,string
                :id ,request-id
                :k ,kont)))))

(defun metta-current-connection ()
  ;; 
  ;; TODO: support multiple
  ;; - not merely project wide, as a  lesson from cider
  (get-buffer "*metta-repl*"))

(defun metta-eval-string (s)
  (let ((output)
        (connection (get-buffer "*metta-repl*"))
        (timout (run-at-time
                 1
                 nil
                 (lambda ()
                   (setq output "timeout")))))
    (unless connection
      (user-error
       "No inferior metta."))
    (metta-eval
     connection
     s
     (lambda (s) (setq output s)))
    (while (null output)
      (accept-process-output
       (get-buffer-process connection)
       0.1))
    (setq myoutput output)
    (if (equal output ">")
        ":>"
      output)))

;; ----------------------
;; set up lispy
;; ----------------------
(when
    (require 'lispy nil t)
  ;; 
  (add-to-list
   'lispy-parens-preceding-syntax-alist
   '(metta-mode . ("[`'~@]+"
                   "#"
                   "#\\?@?"
                   "\\!"
                   "\\!\\s-+?"
                   "\\$")))
  ;; ------------------------------------------
  (add-to-list 'lispy-eval-alist '((metta-mode) lispy metta-eval-string)))

(defun metta-last-sexp ()
  ;; (thing-at-point 'sexp)
  ;; ?? 
  ;; dependency on lispy
  ;; maybe dependency on lispy is ok? 
  (lispy--string-dwim))

(defun metta-eval-last-sexp ()
  (interactive)
  (metta-eval
   (metta-current-connection)
   (metta-last-sexp)
   (lambda (s)
     (message "%s" s))))


(provide 'metta-mode)



