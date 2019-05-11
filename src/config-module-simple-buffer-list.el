;; -*- lexical-binding: t -*-

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

;;
;; Simple Buffer Listing
;;
;; TODO: Documentation
;;

(require 'iflipb)

(defun bl:visit-buffer-at-current-line ()
  "Visits the buffer at the current line.  The buffer name is
denoted by current text field at the current cursor."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-char)
    (let* ((buffer-name (s-trim (field-string-no-properties)))
           (buffer (get-buffer buffer-name)))
      (if (null buffer)
          (message "Buffer %s doesn't exist" buffer-name)
        (progn
          (kill-current-buffer)
          (switch-to-buffer buffer))))))

(defun bl:show-buffer-chooser ()
  "Shows the buffer chooser."
  (interactive)
  (let ((buffer-list (iflipb-interesting-buffers))
        (buffer-index iflipb-current-buffer-index)
        (current-buffer (get-buffer-create "*buffer-list*")))
    (with-current-buffer current-buffer
      (~clean-up-buffer)

      (dolist (buffer buffer-list)
        (let ((name (buffer-name buffer))
              (path (if (buffer-file-name buffer)
                        (buffer-file-name buffer)
                      "")))
          (insert (propertize (format "%-60s" name)
                              'field name)
                  (propertize path
                              'field (s-concat "path-" name)))
          (newline)))

      (goto-line buffer-index)
      (beginning-of-line)

      (evil-normal-state)
      (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer)
      (evil-define-key 'insert 'local (kbd "q") #'kill-current-buffer)
      (evil-define-key 'normal 'local (kbd "RET") #'bl:visit-buffer-at-current-line)
      (evil-define-key 'insert 'local (kbd "RET") #'bl:visit-buffer-at-current-line)
      (evil-define-key 'insert 'local (kbd "<double-mouse-1>") #'bl:visit-buffer-at-current-line)
      (evil-define-key 'normal 'local (kbd "<double-mouse-1>") #'bl:visit-buffer-at-current-line)

      (switch-to-buffer current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring simple buffer listing")

(provide 'rmacs:config-module-simple-buffer-list)