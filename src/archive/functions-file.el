;;
;; Copyright (C) 2017 Ha-Duong Nguyen (@cmpitg)
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

(defun ~list-dir (path)
  "List a directory content."
  (directory-files path))

(defun ~list-dir-full-path (path)
  "List a directory content with full path."
  (-map (lambda (file)
          (s-concat path "/" file)) (directory-files path)))

(defun ~is-directory? (&optional text)
  "Determine if a portion of text is a directory on the
filesystem."
  (interactive)
  (let ((text (cond ((stringp text)
                     text)
                    ((~is-selecting? text)
                     (~current-selection))
                    (t
                     (read-string "Path: ")))))
    (f-dir? text)))

(defun ~current-dir ()
  "Current directory."
  (or (file-name-directory (or load-file-name buffer-file-name ""))
      "~"))

(defun ~current-file-full-path ()
  "Full path to current file."
  (or (expand-file-name buffer-file-name)
      ""))

(defun ~current-file-name ()
  "Current file name."
  (if buffer-file-name
    (concat (file-name-base buffer-file-name) "."
            (file-name-extension buffer-file-name))
    ""))

(defun ~current-file-name-without-extension ()
  "Return current file name without extension."
  (file-name-sans-extension (~current-file-name)))

(defalias '~open-file 'find-file
  "Open a file")

(defalias '~open-file-other-window 'find-file-other-window
  "Open a file in other window")

(defalias '~file-exists? 'file-exists-p
  "Determine if a file exists")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading file-related functions")
(provide 'ee:functions-file)
