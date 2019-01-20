;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2019 Ha-Duong Nguyen (@cmpitg)
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
;; Inserting eval result back to the current buffer after the expression.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables & parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar eval-result-insertion:*insert-eval-result-enabled* nil
  "Determining if the result of an `eval' function should be
inserted back to the current buffer after current point.")

(defvar eval-result-insertion:*result-length-threshold* 200
  "The maximum number of characters under which the eval result
stays as-is when inserted back to the buffer.  If the number of
characters exceeds this threshold, insert an elipsis.")

(defvar eval-result-insertion:*begin-token* "⇨"
  "The token which is used as the marker to determine which point
is the beginning of the eval result.")

(add-to-list 'safe-local-variable-values
             '(eval-result-insertion:*insert-eval-result-enabled* . t))
(add-to-list 'safe-local-variable-values
             '(eval-result-insertion:*result-length-threshold* . t))
(add-to-list 'safe-local-variable-values
             '(eval-result-insertion:*begin-token* . t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* eval-result-insertion:add-eval-result (result
                                               &key
                                               (max-length eval-result-insertion:*result-length-threshold*))
  "Adds eval result after the current
`eval-result-insertion:*begin-token*'.  The token should be
prefixed by a double line comment and a space, in the next line.
If the token is not yet found, it would be inserted.  There must
be at least one blank line between the currently eval'ed
expression and the next one."
  (interactive "MValue: ")
  (save-excursion
    (let* ((prefix (s-concat (s-repeat 2 comment-start) " "))
           (result-str (if (stringp result) result (format "%s" result)))
           (short-result (if (> (length result-str) max-length)
                             "..."
                           result-str))
           (final-result (s-replace-regexp (rx bol) prefix short-result)))
      (when (= (point) (save-excursion (beginning-of-line) (point)))
        (previous-line)
        (end-of-line))
      (cond ((save-excursion (next-line)
                             (beginning-of-line)
                             (looking-at
                              (rx bol
                                  (0+ " ")
                                  (1+ (eval comment-start))
                                  " " (eval eval-result-insertion:*begin-token*)
                                  (0+ " "))))
             (next-line)
             (next-line)
             (beginning-of-line)
             (insert final-result)
             (let ((begin-pos (point)))
               (search-forward-regexp (rx bol eol))
               (delete-region begin-pos (1- (point)))))
            (t
             (next-line)
             (beginning-of-line)
             (insert prefix "⇨" "\n"
                     final-result "\n"))))))

(defun eval-result-insertion:add-eval-result-at-point (value)
  "Advice to add the eval result at the current point."
  (when (and (boundp 'eval-result-insertion:*insert-eval-result-enabled*)
             eval-result-insertion:*insert-eval-result-enabled*)
    (save-excursion (eval-result-insertion:add-eval-result value))))

(defun eval-result-insertion:add-eval-result-at-end-of-defun (value)
  "Advice to add the eval result at the end of the `defun' form."
  (when (and (boundp 'eval-result-insertion:*insert-eval-result-enabled*)
             eval-result-insertion:*insert-eval-result-enabled*)
    (save-excursion
      (end-of-defun)
      (eval-result-insertion:add-eval-result value))))

(defun eval-result-insertion:add-eval-result-at-current-sexp (value)
  "Advice to add the eval result at the end of the current sexp."
  (when (and (boundp 'eval-result-insertion:*insert-eval-result-enabled*)
             eval-result-insertion:*insert-eval-result-enabled*)
    (save-excursion
      (sp-up-sexp)
      (end-of-defun)
      (eval-result-insertion:add-eval-result value))))

(defun eval-result-insertion:add-eval-result-at-end-of-selection (value)
  "Advice to add the eval result at the end of the current
selection."
  (when (and (boundp 'eval-result-insertion:*insert-eval-result-enabled*)
             eval-result-insertion:*insert-eval-result-enabled*)
    (save-excursion
      (when (~is-selecting?)
        (region-end))
      (eval-result-insertion:add-eval-result value))))

(defun eval-result-insertion:toggle-eval-result-insertion ()
  "Enables/disables the insertion of eval result."
  (interactive)
  (setq eval-result-insertion:*insert-eval-result-enabled* (not eval-result-insertion:*insert-eval-result-enabled*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up & tearing down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-result-insertion:add-advice ()
  "Adds advice to enable the insertion of eval result."
  (interactive)
  (advice-add 'eval-last-sexp :filter-return #'eval-result-insertion:add-eval-result-at-point)
  (advice-add 'pp-eval-last-sexp :filter-return #'eval-result-insertion:add-eval-result-at-point)
  (advice-add '~eval-current-sexp :filter-return #'eval-result-insertion:add-eval-result-at-current-sexp)
  (advice-add '~eval-region :filter-return #'eval-result-insertion:add-eval-result-at-end-of-selection)
  (advice-add 'eval-defun :filter-return #'eval-result-insertion:add-eval-result-at-end-of-defun))

(defun eval-result-insertion:remove-advice ()
  "Removes advice to disable the insertion of eval result."
  (interactive)
  (advice-remove 'eval-last-sexp #'eval-result-insertion:add-eval-result-at-point)
  (advice-remove 'pp-eval-last-sexp #'eval-result-insertion:add-eval-result-at-point)
  (advice-remove '~eval-current-sexp #'eval-result-insertion:add-eval-result-at-current-sexp)
  (advice-remove '~eval-region #'eval-result-insertion:add-eval-result-at-end-of-selection)
  (advice-remove 'eval-defun #'eval-result-insertion:add-eval-result-at-end-of-defun))

(eval-result-insertion:add-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring insertion of eval result")

(provide 'rmacs:config-module-eval-result-insertion)
