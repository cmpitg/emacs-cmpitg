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
  "Visits the buffer at the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-char)
    (let* ((buffer-name (s-trim (get-text-property (point) 'field)))
           (buffer (get-buffer buffer-name)))
      (if (null buffer)
          (message "Buffer %s doesn't exist" buffer-name)
        (progn
          (kill-current-buffer)
          (switch-to-buffer buffer))))))

(defun bl:kill-one-or-more-buffers ()
  "Kills the buffers from the curren region.  If there is no
active region, kill the buffer at the current line."
  (interactive)
  (save-excursion
    (let* ((first-line-number (line-number-at-pos (region-beginning)))
           (last-line-number (line-number-at-pos (region-end)))
           (line-counter (1+ (- last-line-number first-line-number))))
      (when (y-or-n-p "Kill buffers? ")
        (loop for line-number from first-line-number to last-line-number
              for current-point = (point-at-bol (goto-line line-number))
              for buffer = (get-buffer (s-trim (get-text-property (point) 'field)))
              unless (null buffer)
              do (kill-buffer buffer))
        (goto-line first-line-number)
        (beginning-of-line)
        (kill-line line-counter)))))

(defun bl:show-buffer-chooser ()
  "Shows the buffer chooser."
  (interactive)
  (let ((buffers (let ((res (iflipb-interesting-buffers)))
                   (concatenate 'list
                                res
                                (loop for b in (buffer-list)
                                      unless (member b res)
                                      collect b))))
        (buffer-index iflipb-current-buffer-index)
        (current-buffer (get-buffer-create "*buffer-list*")))
    (with-current-buffer current-buffer
      (~clean-up-buffer)

      (dolist (buffer buffers)
        (let ((name (buffer-name buffer))
              (path (buffer-file-name buffer)))
          (insert (if path
                      (propertize path 'field name)
                    (propertize name 'field name)))
          (newline)))

      (goto-line buffer-index)
      (beginning-of-line)

      ;; (bind-key "q" #'kill-current-buffer (current-local-map))
      ;; (bind-key "RET" #'bl:visit-buffer-at-current-line (current-local-map))
      ;; (bind-key "<double-mouse-1>" #'bl:visit-buffer-at-current-line (current-local-map))
      (local-set-key (kbd "q") #'kill-current-buffer)
      (local-set-key (kbd "RET") #'bl:visit-buffer-at-current-line)
      (local-set-key (kbd "<double-mouse-1>") #'bl:visit-buffer-at-current-line)

      (with-eval-after-load "evil"
        (evil-normal-state)
        (evil-define-key 'normal 'local (kbd "d") #'bl:kill-one-or-more-buffers)
        (evil-define-key 'insert 'local (kbd "d") #'bl:kill-one-or-more-buffers)
        (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer)
        (evil-define-key 'insert 'local (kbd "q") #'kill-current-buffer)
        (evil-define-key 'normal 'local (kbd "RET") #'bl:visit-buffer-at-current-line)
        (evil-define-key 'insert 'local (kbd "RET") #'bl:visit-buffer-at-current-line)
        (evil-define-key 'insert 'local (kbd "<double-mouse-1>") #'bl:visit-buffer-at-current-line)
        (evil-define-key 'normal 'local (kbd "<double-mouse-1>") #'bl:visit-buffer-at-current-line))

      (switch-to-buffer current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring simple buffer listing")

(provide 'rmacs:config-module-simple-buffer-list)
