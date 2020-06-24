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
;; Rocket the Command Executor
;;
;; TODO: Documentation
;;

(require 'dash)
(require 's)

(defvar rocket:*command-runner-buffer-name*
  "*command-executor*")

(defvar rocket:*history-path*
  (expand-file-name "~/.local/app-runner-history"))

(defvar rocket:*default-exec-prefix*
  "!@")

(defvar rocket:*history-max*
  256)

(defun rocket:save-to-history (text)
  "Saves the current text to history."
  (~exec-sh (list "add-to-history" "--max-history" 
                  rocket:*history-max*
                  rocket:*history-path*)
            :stdin text))

(defun rocket:insert-history (history-path)
  "Inserts history to the current buffer."
  (insert-file history-path))

(defun rocket:run-text-from-context ()
  "Runs text from the current context."
  (interactive)
  (let ((text (if (region-active-p)
                  (~get-selection)
                (s-trim (thing-at-point 'line)))))
    (rocket:save-to-history text)
    (~execute text)))

(defun rocket:run-text-from-context-then-delete-frame ()
  "Runs text from the current context then delete the current frame."
  (interactive)
  (let ((res (rocket:run-text-from-context)))
    (delete-frame)
    res))

(defun* rocket:get-paths (&optional (path-env-var-name "PATH"))
  (thread-last (getenv path-env-var-name)
    (s-split ":")))

(defun* rocket:get-executables (path)
  "Gets the list of executables with relative to a path."
  (when (file-exists-p path)
    (let* ((default-directory path)
           (find-command (executable-find "find"))
           (strip-from (length "./"))
           (lines (thread-last (~exec-sh (list find-command "." "-maxdepth" "1" "-type" "f,l" "-executable"))
                    string-trim
                    s-lines)))
      (loop for line in lines
            unless (string-empty-p line)
            collect (substring line strip-from)))))

(defun rocket:insert-all-execs (paths)
  (interactive)
  (dolist (path paths)
    (loop for exec in (rocket:get-executables path)
          unless (null exec)
          do (insert (concat rocket:*default-exec-prefix* " " exec) "\n"))))

(defun rocket:show-command-runner ()
  "Shows command runner."
  (interactive)
  (let ((current-buffer (get-buffer-create rocket:*command-runner-buffer-name*)))
    (with-current-buffer current-buffer
      (~clean-up-buffer)
      (goto-char (point-min))
      (rocket:insert-history rocket:*history-path*)

      (goto-char (point-max))
      (rocket:insert-all-execs (rocket:get-paths))

      (delete-duplicate-lines (point-min) (point-max))

      (goto-char (point-min))
      (evil-normal-state)
      (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer)
      
      (evil-define-key 'insert 'local (kbd "<S-return>") #'rocket:run-text-from-context)
      (evil-define-key 'normal 'local (kbd "<S-return>") #'rocket:run-text-from-context))
    (switch-to-buffer current-buffer)))

(defun rocket:show-command-runner-with-dedicated-frame ()
  "Shows command runner frame.  After running a command, the
frame closes itself."
  (interactive)
  (with-current-buffer (rocket:show-command-runner)
    (setq-local local/delete-frame-on-close (selected-frame))
    ;; (~wmii/set-frame-floating (selected-frame))
    ;; (~center-frame 1200 700)
    (evil-define-key 'insert 'local (kbd "<S-return>") #'rocket:run-text-from-context-then-delete-frame)
    (evil-define-key 'normal 'local (kbd "<S-return>") #'rocket:run-text-from-context-then-delete-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Rocket the command executor")

(provide 'rmacs:config-module-rocket)
