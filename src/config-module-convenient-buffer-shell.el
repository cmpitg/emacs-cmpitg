;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2019-2020 Ha-Duong Nguyen (@cmpitg)
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
;; TODO: Doc about bs:local/prompt-regexp
;;

(defvar bs:*output-beginning-marker* "### ««« ###"
  "String that marks the beginning of the output from the interpreter.")

(defvar bs:*output-end-marker* "### »»» ###"
  "String that marks the end of the output from the interpreter.")

(defvar bs:*delete-new-prompt?* t
  "Boolean value, determines whether or not the new
  prompt (emitted by the interpreter after each command) is
  deleted.")

(add-to-list 'safe-local-variable-values '(bs:local/prompt-regexp . t))

(cl-defun bs:ensure-buffer-process (&key name buffer command prompt-regexp)
  "TODO"
  (if (process-live-p (get-buffer-process buffer))
      (get-buffer-process buffer)
    (save-excursion
      (insert "\n")
      (lexical-let* ((reg (if (boundp 'bs:local/prompt-regexp)
                              bs:local/prompt-regexp
                            prompt-regexp))
                     (p (make-process
                         :name name
                         :buffer buffer
                         :command command
                         :filter #'(lambda (proc str)
                                     (let ((b (process-buffer proc)))
                                       ;; b and buffer should be the same
                                       (when (buffer-live-p b)
                                         (with-current-buffer b
                                           (let* ((moved? (= (point) (process-mark proc))))
                                             (save-excursion
                                               ;; Go to the place where output
                                               ;; was lasted written
                                               (goto-char (process-mark proc))

                                               ;; Insert new output, try to colorize
                                               (insert (ansi-color-apply (s-replace "\r" "" str)))

                                               ;; Usually an interpreter emits
                                               ;; the prompt when the command
                                               ;; has finished running.  We
                                               ;; would like to not write that
                                               ;; prompt to the buffer
                                               (when (looking-back reg nil nil)
                                                 (when bs:*delete-new-prompt?*
                                                   (let ((beg (match-beginning 0))
                                                         (end (match-end 0)))
                                                     (delete-region beg end))
                                                   (insert bs:*output-end-marker* "\n"))

                                                 ;; Go to the command
                                                 (search-backward-regexp bs:*output-beginning-marker*)
                                                 (previous-line))

                                               ;; Record the place where the current output ends
                                               (set-marker (process-mark proc) (point)))
                                             (when moved? (goto-char (process-mark proc)))))))))))
        (set-marker (process-mark p) (point))
        p))))

(cl-defun bs:send-string (str &optional (buffer (current-buffer)))
  "TODO"
  (when (buffer-live-p buffer)
    (when-let (p (get-buffer-process buffer))
      ;; Delete current output block
      (when (and bs:*delete-new-prompt?*
                 (save-mark-and-excursion
                   (ignore-errors
                     (next-line)
                     (beginning-of-line)
                     (looking-at (rx bol (0+ space)
                                     (eval bs:*output-beginning-marker*)
                                     (0+ space) eol)))))
        (call-interactively '~delete-output-block)
        (kill-line))

      (insert bs:*output-beginning-marker* "\n")
      (set-marker (process-mark p) (point))
      (process-send-string p str))))

(cl-defun bs:send-complete-string (str &optional (buffer (current-buffer)))
  "TODO"
  (bs:send-string (s-concat str "\n") buffer))

(cl-defun bs:send-region (&optional (buffer (current-buffer)))
  "TODO"
  (interactive)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (region-active-p)
        (bs:send-string (buffer-substring (region-beginning) (region-end))
                        buffer)))))

(cl-defun bs:send-complete-region (&optional (buffer (current-buffer)))
  "TODO"
  (interactive)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (region-active-p)
        (let* ((str (s-concat (buffer-substring (region-beginning) (region-end))
                              "\n")))
          (goto-char (region-end))
          (insert "\n")
          (bs:send-string str buffer))))))

(cl-defun bs:ensure-buffer-process-shell (&key (name (s-concat "shell::" (buffer-name)))
                                             (buffer (current-buffer))
                                             (shell "bash")
                                             (prompt-regexp (rx bol
                                                                (0+ (not (any "$")))
                                                                "$"
                                                                (0+ " "))))
  "TODO"
  (interactive)
  (bs:ensure-buffer-process :name name
                            :buffer buffer
                            :command (list "/usr/bin/env" "TERM=dumb" "PAGER=cat" shell)
                            :prompt-regexp prompt-regexp))

(cl-defun bs:exec (str &optional (buffer (current-buffer)))
  "TODO"
  (bs:ensure-buffer-process-shell)
  (bs:send-complete-string str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring convenient buffer shell module")

(provide 'rmacs:config-module-convenient-buffer-shell)
