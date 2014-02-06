;;
;; Copyright (C) 2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
;;
;; This project is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This project is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

(defun ~eval-string (str)
  "Eval a string."
  (interactive)
  (eval (read str)))

(defun ~insert-into-emacs-lisp-docstring (string)
  "Interactive command.  Prompt and insert a string and escape it
as Emacs Lisp docstring format.

E.g.

\(-insert-into-emacs-lisp-docstring \"\(message \\\"hola mundo!\\\"\)\"\)
"
  (interactive "MInput your string: ")
  (insert (s-replace-all '(("\\" . "\\\\\\\\")
                           ("(" . "\\\\(")
                           (")" . "\\\\)")
                           ("\"" . "\\\\\""))
                         string)))

(defun ~add-bracket-and-eval (&optional string)
  "Add outer-most surrounding bracket if necessary and eval the
string.  This function may be called interactively.  If it's in
interactive mode and there's current a selection, the selection
is evaluted.

This function is convenient when being called interactively or
quickly eval a selection which contains Emacs Lisp code.

E.g.

\(-add-bracket-and-eval \"message \\\"¡Hola mundo!\\\"\"\)
;; => ¡Hola mundo!

\(-add-bracket-and-eval \"\(message \\\"¡Hola mundo!\\\"\)\"\)
;; => ¡Hola mundo!
"
  (interactive)
  (let* ((preprocessed-sexp (cond ((not (-string-empty? string))
                                   string)
                                  ((is-selecting?)
                                   (get-selection))
                                  (t
                                   (read-string "Command: "))))
         (sexp (if (not (and (s-starts-with? "(" preprocessed-sexp)
                             (s-ends-with?   ")" preprocessed-sexp)))
                 (format "(%s)" preprocessed-sexp)
                 preprocessed-sexp)))
    (~eval-string sexp)))

(defun ~add-load-path (path)
  "Add path to load-path."
  (add-to-list 'load-path path))

(defun ~save-macro (name)
  "Take a name as argument and save the last defined macro."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)                ; Use this name for the macro
  (find-file *saved-macro-path*)               ; Load the macro file
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)                      ; Copy the macro
  (newline)
  (save-buffer)
  (kill-buffer))

(defun ~is-var-defined? (symbol)
  "Check if the variable corresponding to the symbol is defined.

E.g.

\(~is-var-defined? 'a-random-symbol-unlikely-to-be-defined\)  ; => nil
\(~is-var-defined? 'c-mode-map\)                              ; => t"
  (boundp symbol))

(defun ~is-function-defined? (symbol)
  "Check if the function corresponding to the symbol is defined.

E.g.
\(~is-function-defined? 'a-random-symbol-unlikely-to-be-defined\)  ; => nil
\(~is-function-defined? '$is-function-defined?\)                   ; => t"
  (fboundp symbol))

(defmacro ~nonnil-or-get-selection-or (var &rest body)
  "Short form for

\(cond \(var
       var\)
      \(\(~is-selecting?\)
       \(~get-selection\)\)
      \(t
       body\)\)

Useful when building interactive functions.

E.g.

\(defun _say-something \(&optional var\)
  \(interactive\)
  \(let \(\(var \(~nonnil-or-get-selection-or var
                                          \(read-string \"Var? \"\)\)\)\)
    \(message-box \"%s\" var\)\)\)
"
  `(cond (,var
          ,var)
         ((~is-selecting?)
          (~get-selection))
         (t
          ,@body)))

(defalias 'eval-string '~eval-string)
(defalias 'insert-into-emacs-lisp-docstring '~insert-into-emacs-lisp-docstring)
(defalias 'add-bracket-and-eval '~add-bracket-and-eval)
(defalias 'add-load-path '~add-load-path)
(defalias 'save-macro '~save-macro)
(defalias 'is-var-defined? '~is-var-defined?)
(defalias 'is-function-defined? '~is-function-defined?)
(defalias 'nonnil-or-get-selection-or '~nonnil-or-get-selection-or)
