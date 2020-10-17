;;
;; Copyright (C) 2020 Ha-Duong Nguyen (@cmpitg)
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
;; Eshell utilization
;;

(defun ~eshell-dir (dir)
  "Starts Eshell in a directory.  If Eshell has already been
started, change the directory."
  (interactive "DDirectory: ")
  (cond
   ((get-buffer "*eshell*")
    (with-current-buffer "*eshell*"
      (cd dir)
      (eshell-emit-prompt))
    (switch-to-buffer "*eshell*"))
   (t
    (cd dir)
    (eshell))))

(defun ~eshell-current-project-dir ()
  "Starts Eshell in the current project directory or the current
directory."
  (interactive)
  (~eshell-dir (~get-current-project-root)))

(defun ~eshell-quit ()
  "Quits Eshell when current command is empty."
  (interactive)
  (insert "exit")
  (eshell-send-input))

(defun ~my/eshell-prompt-function ()
  (format " - %s %s@%s %s -\n%s "
          (format-time-string "%H:%M:%S" (current-time))
          (user-login-name)
          (system-name)
          (eshell/pwd)
          (if (zerop (user-uid)) "#" "$")))

;; (defun ~my/eshell-prompt-function ()
;;   (format " - %s %s@%s %s -\n"
;;           (format-time-string "%H:%M:%S" (current-time))
;;           (user-login-name)
;;           (system-name)
;;           (eshell/pwd)))

(defun ~my/eshell-maybe-bol ()
  "Goes to beginning of command line first, then beginning of
line in Eshell."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (when (= p (point))
      (beginning-of-line))))

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(bind-key "<M-insert>" '~eshell-current-project-dir)

;; (setq eshell-prompt-function #'~my/eshell-prompt-function)
;; (setq eshell-prompt-regexp (rx bol (or "#" "$") " "))
;; (setq eshell-prompt-regexp "")
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (bind-key "s-d" #'~my/eshell-maybe-bol eshell-mode-map)
              (evil-define-key 'normal eshell-mode-map (kbd "0") #'~my/eshell-maybe-bol)))
