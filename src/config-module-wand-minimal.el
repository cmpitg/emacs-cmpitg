;; -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2022 Ha-Duong Nguyen (@cmpitg)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Embedded Wand without the processing of comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'wand-helper:get-selection '~get-selection)
(defalias 'wand-helper:eval-string '~eval-string)
(defalias 'wand-helper:maybe-uncomment-string '~comment)
(defalias 'wand-helper:find '~find)

(defvar wand:*rules*
  '()
  "The list of rules to for pattern-based action.
Each rule is a cons of the format `\(check-fn . action-fn\)`:

* `check-fn` is a one-argument function, taking a string and
  determining if the string satisfies the rule.

* `action-fn` is a one-argument function, taking a string and
  execute the action based on that string if its corresponding
  `check-fn` returns `t'.")

(defun wand:get-rule-action (string)
  "Determines if a string matches a predefined rule.  If it does,
returns the function corresponding to that rule's action;
otherwise returns `nil'."
  (thread-last
    wand:*rules*
    (wand-helper:find (lambda (rule-pair)
                        (let ((rule-check-fn (car rule-pair)))
                          (funcall rule-check-fn string))))
    cdr))

(defun wand:eval-string (&optional string)
  "Adds a pair of surrounding brackets if necessary and evals the
string.  If no string is passed, take the current region as the
string.

This function is convenient when being called interactively or
quickly eval a region.

E.g.

\(wand:eval-string \"message \\\"¡Hola mundo!\\\"\"\)
;; => ¡Hola mundo!

\(wand:eval-string \"\(message \\\"¡Hola mundo!\\\"\)\"\)
;; => ¡Hola mundo!
"
  (interactive)
  (let* ((preprocessed-sexp (if (or (null string)
                                    (string-empty-p string))
                                (wand-helper:get-selection)
                              string))
         (sexp (cond ((and (intern-soft preprocessed-sexp)
                           (boundp (intern-soft preprocessed-sexp)))
                      preprocessed-sexp)
                     ((string-prefix-p "(" (string-trim-left preprocessed-sexp))
                      preprocessed-sexp)
                     (t
                      (format "(%s)" preprocessed-sexp)))))
    (unless (string-empty-p (string-trim string))
      (wand-helper:eval-string sexp))))

(defun* wand:create-rule (&key (skip-comment t)
                               match
                               capture
                               (action wand:eval-string))
  "This function provides a simplified and declarative way to
create a pattern-action rule without having to construct the
`\(check-fn . action-fn\)` conses manually.

`wand:create-rule' takes several arguments describing the process
of matching and extracting a string and the action performed upon
it:

* `match` is a regexp to test whether the input string (passed to
  `check-fn`) satisfies the current rule.  The input string is
  checked with `string-match-p'.

* `capture` determines how the input string is extracted to be
  passed to `action-fn`.  It takes one of the following types of
  value:

  - `:whole' - means the original string is passed to `action`.

  - `:after' - means only substring after the match part is
    passed to `action`.

  - `nil' - means an empty string is passed to `action`.

  - a regular expression _with capture group_.  `string-match'
    followed by `match-string' are called to extract the first
    captured group which is then is passed to `action`.

  - a function - that takes the original string and returns what
    `action` wants to process

* `skip-comment` takes either `t' or `nil', determining the
  string is stripped of comments.  The comment syntax is defined
  in the current major mode.

* `action` is `action-fn`, a function that will be called when
  the input string is matched.  `action` is `wand:eval-string' by
  default.

E.g.

Call `\(message-box something\)` when input string is `#> something`:

  \(wand:create-rule :match \"#> \"
            :capture :after
            :action message-box\)

Browse a HTTP/HTTPS web page when input string is
`http://some-url` or `https://some-url`:

  \(wand:create-rule :match \"https?://\"
            :capture :whole
            :action browse-url\)

Open file when input string is `file:///path/to/your-file`:

  \(wand:create-rule :match \"file:///\"
            :capture :after
            :action find-file\)
"
  (cl-labels ((rule-check-fn
               (str)
               (thread-last (wand-helper:maybe-uncomment-string str skip-comment
                                                                :major-mode-fn major-mode)
                            (string-match-p match)))
              (action-fn
               (str)
               (let* ((str (wand-helper:maybe-uncomment-string str skip-comment
                                                               :major-mode-fn major-mode))
                      (processed-str
                       (cond
                        ((eq :after capture)
                         (let ((prefix (progn (string-match match str)
                                              (match-string 0 str))))
                           (s-chop-prefix prefix str)))

                        ((eq :whole capture)
                         str)

                        ((null capture)
                         nil)

                        ((functionp capture)
                         (funcall capture str))

                        (t
                         (error "`capture` must be :after, :whole, nil, or a function")))))
                 (funcall action processed-str))))
    (cons (function rule-check-fn)
          (function action-fn))))

(cl-defun wand:execute (&optional (string-to-execute ""))
  "Executes a string based on predefined rules stored in
`wand:*rules*.  If no rules are found, eval the string using
`wand:eval-string' function.

This function could be called interactively.  The string to
execute is determined as follow:

* If this function is called non-interactively, it's the argument
  that is passed to this function,

* If there is currently a selection, it's the current selected
  text,

* Otherwise, do nothing.

The rules are defined in `wand:*rules*' variable.  Use
`wand:add-rule' or `wand:add-rule-by-pattern' to add rule,
`wand:remove-rule' or `wand:remove-rule-by-pattern' to remove
rule.

For strings that are not matched by any rules, they're called
with `wand:eval-string' by default.

E.g.

\(some-func \"message \\\"Hello World\\\"\"\)
\(some-func \"\(message \\\"Hello World\\\"\\)\"\)
;; Both echo \"Hello World\" in echo area
"
  (interactive)
  (let* ((string (if (or (null string-to-execute)
                         (string-empty-p string-to-execute))
                     (wand-helper:get-selection)
                   string-to-execute))
         (action (or (wand:get-rule-action string)
                     #'wand:eval-string)))
    (unless (string-empty-p (string-trim string))
      (funcall action string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Wand (minimal)")

(provide 'rmacs:config-module-wand-minimal)
