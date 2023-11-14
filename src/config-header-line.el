;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2018-2021 Ha-Duong Nguyen (@cmpitg)
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
  " "
  "Separator for the header line.")

(defvar *header-line-max-path-length*
  50
  "Number of maximum characters to which the current
`buffer-file-name' is truncated to.")

(setq-default header-line-format
              `(""
                "x"
                ,*header-line-separator*
                "buffer-"
                ,*header-line-separator*
                "w-"
                ,*header-line-separator*
                "w→"
                ,*header-line-separator*
                "w↓"
                ,*header-line-separator*
                ;; "✂"
                ;; ,*header-line-separator*
                ;; "☍"
                ;; ,*header-line-separator*
                ;; "⎀"
                ;; ,*header-line-separator*
                "🔍"
                ,*header-line-separator*
                "<>"
                ,*header-line-separator*
                "esh"
                ,*header-line-separator*
                "M-x"
                ,*header-line-separator*
                "term"
                ,*header-line-separator*
                ;; "＋"
                ;; ,*header-line-separator*
                "exec"
                ,*header-line-separator*
                "B"
                ,*header-line-separator*
                "msgs"
                ,*header-line-separator*
                "┣"
                ,*header-line-separator*
                "✓"
                ,*header-line-separator*
                "✗"
                ,*header-line-separator*
                "⇌"
                ,*header-line-separator*
                (:eval (if (buffer-file-name)
                           (~shorten-string buffer-file-name
                                            *header-line-max-path-length*)
                         ""))
                ;; ,*header-line-separator*
                ;; "+"
                ))

(defun ~header-line-execute (event)
  (interactive "e")
  (let* ((e (event-end event))
         (obj (posn-object e))
         (str (first obj)))
    (cond ((string= (~shorten-string buffer-file-name
                                     *header-line-max-path-length*)
                    str)
           (kill-new buffer-file-name)
           (message "File path %s copied to clipboard" buffer-file-name))
          ((string= "x" str)
           (call-interactively #'delete-frame))
          ((string= "esh" str)
           (call-interactively #'eshell))
          ((string= "Q" str)
           (call-interactively #'keyboard-quit))
          ((string= "buffer-" str)
           (call-interactively #'kill-current-buffer))
          ((string= "✂" str)
           (call-interactively #'cua-cut-region))
          ((string= "☍" str)
           (call-interactively #'cua-copy-region))
          ((string= "⎀" str)
           (call-interactively #'cua-paste))
          ((string= "term" str)
           (call-interactively #'~open-term-emu))
          ((string= "M-x" str)
           (call-interactively #'amx))
          ((string= "🔍" str)
           (call-interactively #'isearch-forward))
          ((string= "<>" str)
           (call-interactively #'er/expand-region))
          ((string= "＋" str)
           (call-interactively #'wand:execute))
          ((string= "exec" str)
           (call-interactively #'~palette/point/exec-sh-in-term-mux-then-pause))
          ((string= "B" str)
           (call-interactively #'~show-buffer-chooser))
          ((string= "msgs" str)
           (switch-to-buffer "*Messages*"))
          ((string= "┣" str)
           (dir-browser:render-dir-buffer default-directory))
          ((string= "✓" str)
           (call-interactively (key-binding (kbd "C-c C-c"))))
          ((string= "✗" str)
           (call-interactively (key-binding (kbd "C-c C-k"))))
          ((string= "⊞+" str)
           (call-interactively #'make-frame))
          ((string= "⊞-" str)
           (call-interactively #'delete-frame))
          ((string= "w-" str)
           (call-interactively #'~delete-window))
          ((string= "w→" str)
           (call-interactively #'split-window-horizontally))
          ((string= "w↓" str)
           (call-interactively #'split-window-vertically))
          ((string= "⇌" str)
           (call-interactively #'~transpose-windows))
          ((string= "git" str)
           (call-interactively #'magit-status))
          ((string= "⮹" str)
           (find-file *toolbox-path*))
          ((string= "+" str)
           (call-interactively #'~header-line-add))
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
