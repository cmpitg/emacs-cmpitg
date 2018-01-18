;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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

(defvar *header-line-separator*
  "'"
  "Separator for the header line.")

(setq-default header-line-format
              `(""
                (:eval (if (buffer-file-name) (buffer-file-name) ""))
                ,*header-line-separator*
                "x"
                ,*header-line-separator*
                "+"))

(defun ~header-line-execute (event)
  (interactive "e")
  (let* ((e (event-end event))
         (obj (posn-object e))
         (str (first obj)))
    (cond ((string= buffer-file-name str)
           (kill-new str)
           (message "File path %s copied to clipboard" str))
          ((string= "x" str)
           (call-interactively 'kill-this-buffer))
          ((string= "+" str)
           (call-interactively '~header-line-add))
          ((string= *header-line-separator* str)
           nil)
          (t
           (wand:execute str)))))

(defun ~header-line-edit (text new-text)
  "Edits an item in the header line.  If `new-text' is empty,
deletes the current `text'."
  (interactive "MCurrent text: \nMNew text: "))

(defun ~header-line-add (text)
  "Adds an item to the end of the header line."
  (interactive "MText: ")
  (unless (string-empty-p text)
    (let ((new-header (thread-first (butlast header-line-format)
                        (append (list text *header-line-separator* "+")))))
      (setq header-line-format new-header))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rmacs:config-header-line)

(message "Finish configuring the header line")
