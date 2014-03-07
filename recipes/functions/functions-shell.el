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

(defun ~popup-shell-command (&optional command)
  "Run a non-interactive shell command and popup a window to
display result."
  (interactive)
  (let* ((command-str (cond ((not (~string-empty? command))
                             command)
                            ((is-selecting?)
                             (get-selection))
                            (t
                             (~read-string "Shell command: ")))))
    (ignore-errors
      (with-current-buffer "*Shell Output*"
        (erase-buffer)))
    (start-process-shell-command "Shell-Command" "*Shell Output*" "ls")
    (~popup-buffer "*Shell Output*")))

(defun ~man-current-word ()
  "`man` this word."
  (interactive)
  (manual-entry (current-word)))

(defun ~exec (command)
  "Execute a shell command then return its value as string."
  (interactive "MCommand: ")
  (shell-command-to-string command))

(defun ~exec-in-other-window (command)
  "Execute in other window."
  (interactive "MCommand: ")
  (shell-command command))

(defun ~exec-then-pipe (command)
  "Execute and pipe output to the current buffer."
  (interactive "MCommand: ")
  (shell-command command t))

(defun ~exec-then-pipe-selection ()
  "Execute selection and pipe output to the current buffer."
  (interactive)
  (~exec-then-pipe (~current-selection)))

(defun ~pipe-then-exec (command)
  "Pipe current region to a command, exec it, and pipe the output back."
  (interactive "MCommand: ")
  (shell-command-on-region (if mark-active (region-beginning) 1)
                           (if mark-active (region-end) 1)
                           command t))

(defalias 'popup-shell-command  '~popup-shell-command)
(defalias 'man-current-word '~man-current-word)
(defalias 'exec '~exec)
(defalias 'exec-in-other-window '~exec-in-other-window)
(defalias 'exec-then-pipe '~exec-then-pipe)
(defalias 'exec-then-pipe-selection '~exec-then-pipe-selection)
(defalias 'pipe-then-exec '~pipe-then-exec)

(defalias '~filter-command '~pipe-then-exec
  "Filter a command")

(defalias '~pipe-then-exec-in-other-window 'shell-command-on-region
  "Filter a command but pipe the other to other window")
