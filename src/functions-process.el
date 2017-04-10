
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

(defun toolbox:run-process (cmd)
  "Runs an external process asynchronously.  The process is not
terminated when Emacs is.  `cmd' is executed via `bash -c'."
  (call-process "bash"
                nil
                0
                nil
                "-c"
                cmd))

(defun ~geiser-repl-process ()
  "Return the process behind current Geiser REPL."
  (let ((repl-buffer (get-buffer "* Racket REPL *")))
    (if repl-buffer
      (get-buffer-process repl-buffer)
      nil)))

(defun ~exec-with-input (command input-text)
  "Executes a shell command with `input-text' piped to stdin and
returns output (+ error) as string."
  (interactive)
  (with-temp-buffer
    (insert input-text)
    (shell-command-on-region (point-min)
                             (point-max)
                             command
                             t
                             t
                             nil)
    (buffer-string)))

(defun ~exec (command)
  "Execute a shell command then return its value as string."
  (interactive "MCommand: ")
  (with-temp-buffer
    (shell-command command t nil)
    (buffer-string)))

(defun ~exec-in-other-window (command)
  "Execute in other window."
  (interactive "MCommand: ")
  (shell-command command))

(defun ~exec< (&optional command)
  "Executes a shell command and pipes the output to the current
buffer.  If there is an active secondary selection active, the
command is the selection string; otherwise, it is read from the
minibuffer.

With prefix argument, always reads command from the minibuffer."
  (interactive)
  (let ((command (or command (~read-command-or-get-from-secondary-selection))))
    (shell-command command t)))

(defun ~exec| (&optional command)
  "Executes a shell command and pipes the output to the current
buffer.  If there is an active primary selection, it is piped as
input to the command and the output from the command would
replace the selection.  If there is an active secondary selection
active, the command is the selection string; otherwise, it is
read from the minibuffer."
  (interactive)
  (let ((command (or command
                     (~read-command-or-get-from-secondary-selection))))
    (if (~is-selecting?)
        (shell-command-on-region (region-beginning)
                                 (region-end)
                                 command
                                 t
                                 t
                                 nil)
      (shell-command command t))))

(defun ~exec> (&optional command)
  "Executes a shell command and pipes the output a pop-up buffer
named \"*Shell Output*\".  If there is an active secondary
selection active, the command is the selection string; otherwise,
it is read from the minibuffer."
  (interactive)
  (let ((command (or command
                     (~read-command-or-get-from-secondary-selection))))
    (get-buffer-create "*Shell Output*")
    (with-current-buffer "*Shell Output*"
      (insert (shell-command-to-string command)))
    (~popup-buffer "*Shell Output*")))

(defun ~exec|-select-output ()
  "Calls `~exec|'.  After the output has been piped in to the
buffer, select it."
  (interactive)
  (let ((marker-start (copy-marker (or (region-beginning) (point)) nil))
        (marker-end (copy-marker (or (region-end) (point)) t)))
    (call-interactively '~exec|)
    (deactivate-mark t)
    (set-mark marker-end)
    (goto-char marker-start)
    (setq deactivate-mark nil)))

(defun ~exec<-select-output (&optional command)
  "Calls `~exec<'.  After the output has been piped in to the
buffer, select it."
  (interactive)
  (let ((marker-start (copy-marker (point) nil))
        (marker-end (copy-marker (point) t)))
    (call-interactively '~exec<)
    (set-mark marker-end)
    (goto-char marker-start)
    (setq deactivate-mark nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '~filter-command '~exec|
  "Filter a command")

(defalias '~filter-command-other-window 'shell-command-on-region
  "Filter a command but pipe the other to other window")

(defalias 'srun 'emamux:send-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading process manipulation functions")
(provide 'ee:functions-process)
