
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

(defun* toolbox:run-process (cmd &key (async t))
  "Runs an external process.  If `async' is non-`nil' The process
is not terminated when Emacs is and the output is discarded;
otherwise, both output from stdout and stderr are direceted to
the \"*Messages*\" buffer. `cmd' is executed via `dash -c'."
  (call-process "dash"
                nil
                (if async 0 "*Messages*")
                nil
                "-c"
                cmd))

(defun* ~run-process (command &key (async t))
  "Runs an external process.  If `async' is non-`nil' the process
is not terminated when Emacs exits and the output is discarded;
otherwise, both output from stdout and stderr are direceted to
the buffer naming after `command'.  `command' is executed via the
current user shell, define by the `SHELL' environment variable."
  (let ((current-shell (getenv "SHELL"))
        (process-name command))
    (if async
        (async-start-process process-name
                             current-shell
                             #'(lambda (process)
                                 (let ((output (~get-process-output process)))
                                   (with-current-buffer (get-buffer-create process-name)
                                     (insert output))))
                             "-c"
                             command)
      (call-process current-shell

                    ;; Taking no input
                    nil

                    ;; Output buffer
                    process-name

                    ;; Don't display output buffer
                    nil

                    ;; Arguments
                    "-c" command))))

(defun ~geiser-repl-process ()
  "Return the process behind current Geiser REPL."
  (let ((repl-buffer (get-buffer "* Racket REPL *")))
    (if repl-buffer
      (get-buffer-process repl-buffer)
      nil)))

(defun ~exec (command)
  "Executes a shell command then returns its value as string."
  (interactive "MCommand: ")
  (with-temp-buffer
    (shell-command command t nil)
    (buffer-string)))

(defun* ~exec (command &key (on-region nil))
  "Executes a shell command then returns its value as string."
  (interactive "MCommand: ")
  (if on-region
      (progn
        (let ((inhibit-message t))
          (shell-command-on-region (region-beginning)
                                   (region-end)
                                   command

                                   ;; Output buffer name
                                   command

                                   ;; Don't replace current region
                                   nil

                                   ;; Error piped to output
                                   nil))
        (~get-buffer-content command))
    (with-temp-buffer
      (shell-command command t nil)
      (buffer-string))))

(defun* ~exec-pop-up (command)
  "Executes a command & pops up a temporary buffer showing
result.  The command is executed asynchronously in a shell which
is determined by the `SHELL' environment variable."
  (interactive "MCommand: ")
  (let ((current-shell (getenv "SHELL"))
        (process-name command))
    (async-start-process process-name
                         current-shell
                         #'(lambda (process)
                             (let ((output (~get-process-output process)))
                               (~popup-message output
                                               :buffer-name process-name)))
                         "-c"
                         command)))

(defun* ~exec< (command)
  "Executes a command and replaces the region with the output.
This function also returns the exit code of the command.  The
command is executed asynchronously in a shell which is determined
by the `SHELL' environment variable."
  (interactive "MCommand: ")
  (let ((current-shell (getenv "SHELL"))
        (process-name command)
        (buffer (current-buffer)))
    (async-start-process process-name
                         current-shell
                         #'(lambda (process)
                             (let ((output (~get-process-output process)))
                               (with-current-buffer buffer
                                 (when (~is-selecting?)
                                   (delete-region (region-beginning)
                                                  (region-end)))
                                 (push-mark)
                                 (insert output))))
                         "-c"
                         command)))

(defun ~exec> (&optional command)
  "Executes a command, taking input from the current region,
pops up a temporary buffer showing result, and returns the exit
code of the command.  The command is executed synchronously in a
shell which is determined by the `SHELL' environment variable."
  (interactive "MCommand: ")
  (prog1 (let ((inhibit-message t))
           (shell-command-on-region (region-beginning)
                                    (region-end)
                                    command

                                    ;; Output buffer name
                                    command

                                    ;; Don't replace current region
                                    nil

                                    ;; Error piped to output
                                    nil))
    (~popup-buffer command)))

(defun ~exec| (&optional command)
  "Executes a command, taking input from the current region,
and replaces the region with the output.  This function also
returns the exit code of the command.  The command is executed
synchronously in a shell which is determined by the `SHELL'
environment variable."
  (interactive "MCommand: ")
  (let ((inhibit-message t))
    (shell-command-on-region (region-beginning)
                             (region-end)
                             command

                             ;; Output buffer name
                             command

                             ;; Don't replace current region
                             t

                             ;; Error piped to output
                             nil)))

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
