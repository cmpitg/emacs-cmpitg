;; -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2022 Ha-Duong Nguyen (@cmpitg)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Omni command palette
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~with-output-to-next-window (goto-beginning? &rest body)
  "Runs BODY with the current window being the next window"
  (when (= 1 (~count-windows))
    (error "Must have must than one window to output"))

  `(with-current-buffer (window-buffer (next-window))
     (when ,goto-beginning?
       (beginning-of-buffer))
     (open-line 2)
     ,@body))
(put '~with-output-to-next-window 'lisp-indent-function 1)

(defmacro ~with-output-to-buffer-file (path goto-beginning? &rest body)
  "Runs BODY with the current buffer being a visited file."
  `(with-current-buffer (~visit-file ,path)
     (when ,goto-beginning?
       (beginning-of-buffer))
     (open-line 2)
     ,@body))
(put '~with-output-to-buffer-file 'lisp-indent-function 2)

(defun ~palette/ensure-prefix (text prefix)
  "Makes sure text has a prefix."
  (lexical-let* ((text (concat prefix (~palette/trim-garbage text)))
                 (padding (first (s-match (rx bos (0+ blank)) text))))
    (concat padding text)))

;; (~palette/ensure-prefix "         ls \\\n" "$ ")
;; (~palette/ensure-prefix "ls \\\n" "$ ")
;; (~palette/ensure-prefix "ls \\" "$ ")
;; (~palette/ensure-prefix "   $!! hello world !!$ aa" "$ ")
;; (~palette/ensure-prefix "   hello !!! world" "$ ")
;; (~palette/ensure-prefix "         pwd" "$ ")
;; (~palette/ensure-prefix "         ls\\\n" "$ ")

(cl-defun ~palette/trim-garbage (text &optional (garbage-regexp (rx bos (1+ (any " ;!$#")))))
  "Trims garbage from TEXT, tries to make text a command."
  (lexical-let* ((elements (s-split-up-to garbage-regexp text 1))
                 (text (if (> (length elements) 1)
                           (string-trim (second elements))
                         (first elements)))
                 (pass-2 (s-split-up-to (rx bos (>= 2 "/")) text 1))
                 (res (if (> (length pass-2) 1)
                          (string-trim (second pass-2))
                        (first pass-2))))
    res))

;; (~palette/trim-garbage "ls \\\n")
;; (~palette/trim-garbage "   ls \\\n")
;; (~palette/trim-garbage "!!$   ls \\\n")
;; (~palette/trim-garbage "   ls $  sss $\\\n")
;; (~palette/trim-garbage "ls \\\n -1 ~/tmp/")
;; (~palette/trim-garbage "/usr/bin/ls \\\n -1 ~/tmp/")

(defun ~palette/decorate-exec-text-at-point ()
  "Decorates executable text at current point.  Executable
text is multiline text that could be executed with Wand."
  (interactive)
  (when (~palette/trim-garbage (thing-at-point 'exec-text))
    (save-mark-and-excursion
      (goto-char (car (bounds-of-thing-at-point 'exec-text)))
      (lexical-let* ((current-first-line (string-trim-right (thing-at-point 'line)))
                     (line (~palette/ensure-prefix current-first-line "$ ")))
        (unless (string= current-first-line line)
          (kill-line)
          (insert line))))))

(defun ~palette/exec-sh-in-term-mux (&optional cmd)
  "Executes a shell command in a terminal multiplexer."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~dispatch-action (concat "!!! " cmd))))

(defun ~palette/exec-sh-in-term-mux-then-pause (&optional cmd)
  "Executes a shell command in a terminal multiplexer, pauses
after command has finished running."
  (interactive)
  (lexical-let* ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd))
                 (cleansed (s-replace-regexp (rx "\\" "\n") "" cmd)))
    (~add-to-history-file *~exec-history-path* cleansed
                          :max-history *~exec-history-max*)
    (~dispatch-action "!! " cleansed)))

(defun ~palette/exec-sh-in-term-mux-piping-to-sh-output-file (&optional cmd)
  "Executes a shell command in a terminal multiplexer, piping output to the next window."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~with-output-to-buffer-file (~get-project-sh-output-path) t
      (insert (format "$ %s" cmd))
      (beginning-of-line)
      (~prepare-for-output-block t)
      (~dispatch-action "!!! " (~build-|rmacs-tee-cmd cmd)))))

(defun ~palette/exec-sh-piping-to-sh-output-file (&optional cmd)
  "Executes a shell command, piping output to the next window."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~with-output-to-buffer-file (~get-project-sh-output-path) t
      (insert (format "$ %s" cmd))
      (beginning-of-line)
      (~exec-sh<-next-line-separate cmd))))

(defun ~palette/exec-sh-piping-here (&optional cmd)
  "TODO"
  (lexical-let ((text (thread-last (~read-command-or-get-from-selection *~exec-history-path* cmd)
                                   ~palette/trim-garbage)))
    (end-of-thing 'exec-text)
    (~exec-sh<-next-line-separate text
                                  :callback #'(lambda (&rest _args)
                                                (end-of-thing 'exec-text)
                                                (forward-line)
                                                (call-interactively #'~mark-current-output-block)
                                                (call-interactively #'~ansi-colorize-region)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Omni Command Palette")

(provide 'rmacs:config-module-omni-command)
