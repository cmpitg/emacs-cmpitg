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

;; (setq *defun-template*)

(defun ~read-simplified-sexp-as-string (prompt)
  "Read a sexp from minibuffer with completion.  The sexp doesn't
need its top-level brackets.  This function returns a string."
  (interactive)
  (let ((minibuffer-completing-symbol t))
    (read-from-minibuffer prompt
                          nil
                          read-expression-map
                          nil
                          'read-expression-history)))

(defun ~eval-string (str)
  "Eval a string."
  (interactive)
  (eval (read str)))

(defun ~eval-then-replace-last-exp ()
  "Eval region then replace last expression with result."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun ~eval-then-replace ()
  "Eval region then replace region with result."
  (interactive)
  (let ((value (~eval-string (get-selection))))
    (kill-region (selection-start) (selection-end))
    (insert (format "%s" value))))

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
  (let* ((preprocessed-sexp (cond ((not (~string-empty? string))
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

(defun* ~read-string (prompt &key
                             (initial-input         nil)
                             (history               nil)
                             (default-value         nil)
                             (inherit-input-method  nil))
  "An alias of read-string, with keyword arguments.  See
`read-string' documentation for more details.

  Read a string from the minibuffer."
  (read-string prompt
               initial-input
               history
               default-value
               inherit-input-method))

(defun ~emacs-lisp-make-function ()
  "Make a `defun` with current selection at the end of the file."
  (interactive)
  (if (is-selecting?)
    (save-excursion
      (let ((function-name (get-selection))
            (*defun-template* (if (~is-var-defined? '*defun-template*)
                                *defun-template*
                                "
\(defun %s \(\)
  \"\"
  \)
")))
        (~insert-text-at-the-end (format *defun-template* function-name))))))

(defun* ~byte-compile-dir (&optional dir &key (force t))
  "Byte compile all Emacs Lisp files is `dir'."
  (interactive)
  (let* ((dir (cond (dir
                     dir)
                    (t
                     (read-directory-name "Directory: ")))))
    (cond (force 
           (byte-recompile-directory (expand-file-name dir) 0 t))
          (t
           (byte-recompile-directory (expand-file-name dir) 0 nil)))))

(defun ~rebuild-my-config ()
  "Rebuild my config by loading `*config-dir*/build-config.el`."
  (interactive)
  (~load-config-files "build-config.el"))

(defun ~alist-get (alist key)
  "Return just the value associated with the key in an alist."
  (cdr (assoc key alist)))

(defun ~get-library-full-path (library-name)
  "Return the full path to a library."
  (save-excursion
    (find-library library-name)
    (let ((file-path (~current-file-full-path)))
      (kill-buffer)
      file-path)))

(defun ~quick-bindkey-local (&optional key expr)
  "Quickly create keybindings for local buffer.  This command
should only be called interactively."
  (interactive)
  (let ((key (if key
                 key
                 (read-key-sequence "Key: ")))
        (expr  (if expr
                   expr
                   (read-from-minibuffer "Eval: "
                                         nil read-expression-map t
                                         'read-expression-history))))
    (local-set-key key '(lambda ()
                         (interactive)
                         (save-excursion
                           (with-temp-buffer
                             (insert expr)
                             (eval-buffer)))))))

(defun ~add-personal-keybinding (key-binding symbol)
  "Add a key binding to `bind-key' library's
`personal-keybindings'."
  (when (featurep 'bind-key)
    (let ((key-binding (list key-binding))
          (symbol      symbol))
      (add-to-list 'personal-keybindings (list key-binding symbol nil)))))

(defalias 'insert-into-emacs-lisp-docstring '~insert-into-emacs-lisp-docstring)
(defalias 'add-bracket-and-eval '~add-bracket-and-eval)
(defalias 'add-load-path '~add-load-path)
(defalias 'save-macro '~save-macro)
(defalias 'is-var-defined? '~is-var-defined?)
(defalias 'is-function-defined? '~is-function-defined?)
(defalias 'nonnil-or-get-selection-or '~nonnil-or-get-selection-or)
(defalias 'eval-string '~eval-string)
(defalias 'eval-then-replace '~eval-then-replace)
(defalias 'eval-then-replace-last-exp '~eval-then-replace-last-exp)
(defalias '~eval-selection 'eval-region)
(defalias 'eval-selection '~eval-selection)
(defalias 'emacs-lisp-make-function '~emacs-lisp-make-function)
(defalias 'byte-compile-dir '~byte-compile-dir)
(defalias 'rebuild-my-config '~rebuild-my-config)
(defalias 'get-library-full-path '~get-library-full-path)
(defalias 'add-personal-keybinding '~add-personal-keybinding)
(defalias 'read-simplified-sexp-as-string '~read-simplified-sexp-as-string)
