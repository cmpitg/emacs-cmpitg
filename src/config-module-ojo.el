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

;;
;; TODO: Documentation
;;
;; TODO: Bug with Cider -> Cider doesn't return the eval result
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables & parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ojo:*insert-eval-result-enabled* nil
  "Determining if the result of an `eval' function should be
inserted back to the current buffer after current point.")

(defvar ojo:*result-length-threshold* 2000
  "The maximum number of characters under which the eval result
stays as-is when inserted back to the buffer.  If the number of
characters exceeds this threshold, insert an elipsis.")

(defvar ojo:*begin-token* "⇨"
  "The token which is used as the marker to determine which point
is the beginning of the eval result.")

(add-to-list 'safe-local-variable-values
             '(ojo:*insert-eval-result-enabled* . t))
(add-to-list 'safe-local-variable-values
             '(ojo:*result-length-threshold* . t))
(add-to-list 'safe-local-variable-values
             '(ojo:*begin-token* . t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* ojo:add-eval-result (result
                             &key
                             (max-length ojo:*result-length-threshold*))
  "Adds eval result after the current
`ojo:*begin-token*'.  The token should be
prefixed by a double line comment and a space, in the next line.
If the token is not yet found, it would be inserted.  There must
be at least one blank line between the currently eval'ed
expression and the next one."
  (interactive "MValue: ")
  (save-excursion
    (let* ((comment-start (or comment-start ";"))
           (prefix (s-concat (s-repeat 2 comment-start) " "))
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
                                  " " (eval ojo:*begin-token*)
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

(defun ojo:add-eval-result-at-point (value)
  "Advice to add the eval result at the current point."
  (when (and (boundp 'ojo:*insert-eval-result-enabled*)
             ojo:*insert-eval-result-enabled*)
    (save-excursion (ojo:add-eval-result value)))
  value)

(defun ojo:add-eval-result-at-end-of-defun (value)
  "Advice to add the eval result at the end of the `defun' form."
  (when (and (boundp 'ojo:*insert-eval-result-enabled*)
             ojo:*insert-eval-result-enabled*)
    (save-excursion
      (end-of-defun)
      (ojo:add-eval-result value)))
  value)

(defun ojo:add-eval-result-at-current-sexp (value)
  "Advice to add the eval result at the end of the current sexp."
  (when (and (boundp 'ojo:*insert-eval-result-enabled*)
             ojo:*insert-eval-result-enabled*)
    (save-excursion
      (sp-up-sexp)
      (end-of-defun)
      (ojo:add-eval-result value)))
  value)

(defun ojo:add-eval-result-at-end-of-selection (value)
  "Advice to add the eval result at the end of the current
selection."
  (when (and (boundp 'ojo:*insert-eval-result-enabled*)
             ojo:*insert-eval-result-enabled*)
    (save-excursion
      (when (~is-selecting?)
        (region-end))
      (ojo:add-eval-result value)))
  value)

(defun ojo:toggle-local ()
  "Enables/disables the insertion of eval result for the current buffer."
  (interactive)
  (make-local-variable 'ojo:*insert-eval-result-enabled*)
  (setq ojo:*insert-eval-result-enabled* (not ojo:*insert-eval-result-enabled*)))

(defun ojo:enable-local ()
  "Enables the insertion of eval result for the current buffer."
  (interactive)
  (make-local-variable 'ojo:*insert-eval-result-enabled*)
  (setq ojo:*insert-eval-result-enabled* t))

(defun ojo:disable-local ()
  "Disables the insertion of eval result for the current buffer."
  (interactive)
  (make-local-variable 'ojo:*insert-eval-result-enabled*)
  (setq ojo:*insert-eval-result-enabled* nil))

(defun ojo:toggle-global ()
  "Enables/disables the insertion of eval result globally, not
effectively buffers that have enabled the feature previously."
  (interactive)
  (setq ojo:*insert-eval-result-enabled* (not ojo:*insert-eval-result-enabled*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up & tearing down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ojo:enable ()
  "Enables the insertion of eval result."
  (interactive)
  (advice-add 'eval-last-sexp :filter-return #'ojo:add-eval-result-at-point)
  (advice-add 'pp-eval-last-sexp :filter-return #'ojo:add-eval-result-at-point)
  (advice-add '~eval-current-sexp :filter-return #'ojo:add-eval-result-at-current-sexp)
  (advice-add '~eval-region :filter-return #'ojo:add-eval-result-at-end-of-selection)
  (advice-add 'eval-defun :filter-return #'ojo:add-eval-result-at-end-of-defun)
  (advice-add 'cider-eval-last-sexp :filter-return #'ojo:add-eval-result-at-point)
  (advice-add 'cider-eval-defun-at-point :filter-return #'ojo:add-eval-result-at-end-of-defun)
  (advice-add 'cider-eval-sexp-at-point :filter-return #'ojo:add-eval-result-at-end-of-defun))

(defun ojo:disable ()
  "Enables the insertion of eval result."
  (interactive)
  (advice-remove 'eval-last-sexp #'ojo:add-eval-result-at-point)
  (advice-remove 'pp-eval-last-sexp #'ojo:add-eval-result-at-point)
  (advice-remove '~eval-current-sexp #'ojo:add-eval-result-at-current-sexp)
  (advice-remove '~eval-region #'ojo:add-eval-result-at-end-of-selection)
  (advice-remove 'eval-defun #'ojo:add-eval-result-at-end-of-defun)
  (advice-remove 'cider-eval-last-sexp #'ojo:add-eval-result-at-point)
  (advice-remove 'cider-eval-defun-at-point #'ojo:add-eval-result-at-end-of-defun)
  (advice-remove 'cider-eval-sexp-at-point #'ojo:add-eval-result-at-end-of-defun))

(define-minor-mode ojo-mode
  "Local minor mode for Ojo."
  :lighter " Ojo"
  (if ojo-mode
      (ojo:enable-local)
    (ojo:disable-local)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Ojo mode")

(provide 'rmacs:config-module-ojo)
