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
;; URL:
;; Version: 0.01
;;  

(define-derived-mode metta-mode lisp-mode "MeTTa")

(add-to-list 'auto-mode-alist '("\\.metta\\'" . metta-mode))

;; (defvar-local metta-eval-process-handler-hook '())
;; (defvar-local metta-eval-request-table (make-hash-table))

(defvar-local metta--request nil)

(defun metta-connetion-process-filter (proc string)
  (with-current-buffer
      (process-buffer proc)
    (insert string)
    (when-let ((k (plist-get metta--request :k)))
      (funcall k string))
    ;; (when-let*
    ;;     ((req
    ;;       (car (hash-table-values
    ;;             metta-eval-request-table)))
    ;;      (k (plist-get req :k)))
    ;;   (funcall k string))
    ))

(defun metta-run-inferior-metta ()
  ;; todo:
  ;; - make it a repl buffer so you can also type
  ;; - run with some start script
  ;; - implement req->response on metta side
  ;; - make it buffer for large outputs
  ;; - be inspried by clojure nrepl
  (interactive)
  (let* ((default-directory "/home/benj/repos/hyperon-experimental/")
         (p (start-process
             "MeTTA REPL"
             (get-buffer-create
              "*metta-repl*")
             "cargo"
             "run"
             "--bin"
             "metta-repl")))
    (set-process-filter
     p
     #'metta-connetion-process-filter)
    (display-buffer
     (process-buffer p))))

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
      ;; the request id would come from the metta side
      (setq-local
       metta--request
       `(:input ,string
                :id ,request-id
                :k ,kont)))))


;; (metta-eval
;;  (get-buffer "*metta-repl*")
;;  "!(+ 1 2)\n"
;;  (lambda (s) (message "%s" s)))

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
    output))


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

(provide 'metta-mode)
