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
;; Bowser the directory browser
;;
;; TODO: Documentation
;;
;; TODOs:
;; * Support multiple directories (in the same buffer/another buffer)
;; * Suffix the directory path with a slash?
;; * Customize the find command?
;; * (Done) Collapse
;; * Remember the state of the bowser buffer
;; * Remember the state of the corresponding directory
;; * Expand all
;;

(require 'dash)
(require 's)
(require 'thingatpt)

(defvar bowser:*buffer-name-format*
  "*bowser:%s*")

(defvar bowser:*level-padding*
  "  ")

(defvar bowser:*history-path*
  (expand-file-name "~/.local/rmacs.bowser-history"))

(defvar bowser:*history-max*
  256)

(defun bowser:save-to-history (text)
  "Saves the current text to history."
  (~exec (format "add-to-history --max-history %s %s"
                 bowser:*history-max*
                 (shell-quote-argument bowser:*history-path*))
         :stdin text))

(defun bowser:is-dir-expanded? (dir)
  "Checks if the current dir is expanded already."
  (let ((dir (file-name-as-directory dir)))
    (unless (local-variable-p 'local/bowser:dirs-expanded?)
      (setq-local local/bowser:dirs-expanded? (make-hash-table :test #'equal)))
    (gethash dir local/bowser:dirs-expanded?)))

(defun bowser:record-dir-expanded (dir expanded?)
  "Records that the current dir is either expanded or not."
  (let ((dir (file-name-as-directory dir)))
    (unless (local-variable-p 'local/bowser:dirs-expanded?)
      (setq-local local/bowser:dirs-expanded? (make-hash-table :test #'equal)))
    (puthash dir expanded? local/bowser:dirs-expanded?)))

(defun bowser:format-buffer-name (path)
  (format bowser:*buffer-name-format* path))

(defun* bowser:strip-newline-from-string (str)
  (if (string-empty-p str)
      str
    (substring str 0 -1)))

(defun bowser:next-line ()
  "Makes sure there is at least one more line forward and move to that line."
  (interactive)
  (when (>= (line-number-at-pos) (count-lines (point-min) (point-max)))
    (save-excursion
      (goto-char (point-at-eol))
      (~open-line 1)
      (delete-region (point-at-bol) (point-at-eol))))
  (next-line)
  (bowser:get-level (thing-at-point 'line)))

(defun* bowser:construct-find-command (dir &key (max-depth 1)
                                           (type :dir))
  "Constructs a find command that will be called to list
directories (with `type' being `:dir') or files (with `type'
equals `:file')."
  (let ((dir (file-name-as-directory dir)))
    (case type
      (:dir
       (format "%s %s -maxdepth %s -type d | sort -n"
               (executable-find "find")
               (shell-quote-argument dir)
               (shell-quote-argument (number-to-string max-depth))))
      (:file
       (format "%s %s -maxdepth %s -not -type d | sort -n"
               (executable-find "find")
               (shell-quote-argument dir)
               (shell-quote-argument (number-to-string max-depth))))
      (t
       (error "The :type argument must be `:dir or `:file'")))))

(defun* bowser:get-dir-paths (dir &key (max-depth 1))
  "Gets subdirectories under a path."
  (let* ((dir (file-name-as-directory (substitute-in-file-name (expand-file-name dir))))
         (find-command (bowser:construct-find-command dir
                                                      :max-depth max-depth
                                                      :type :dir)))
    (loop for path in (thread-first
                          (~exec find-command)
                        bowser:strip-newline-from-string
                        s-lines
                        rest)
          collect (file-name-as-directory path))))

(defun* bowser:get-file-paths (dir &key (max-depth 1))
  "Gets non-dir files under a path."
  (let* ((dir (file-name-as-directory (substitute-in-file-name (expand-file-name dir))))
         (find-command (bowser:construct-find-command dir
                                                      :max-depth max-depth
                                                      :type :file)))
    (loop for path in (thread-first
                          (~exec find-command)
                        bowser:strip-newline-from-string
                        s-lines)
          unless (string-empty-p path)
          collect path)))

(defun* bowser:insert-paths (dir &key (level 0) (max-depth 1))
  "Inserts paths for a directory at the current place in the current buffer.
If a path is a directory path and has been expanded before, it
will be expanded again.  Directories are inserted before non-dir
files."
  (goto-char (point-at-bol))
  ;; (delete-region (point-at-bol) (point-at-eol))
  (let ((dir (file-name-as-directory dir))
        (padding (apply #'concat (make-list level bowser:*level-padding*))))
    ;; Insert the corresponding subdirectories and expend them if needed
    (loop for path in (bowser:get-dir-paths dir :max-depth max-depth)
          do (progn
               (insert padding path "\n")
               (when (bowser:is-dir-expanded? path)
                 (bowser:insert-paths path :level (1+ level)))))
    ;; Now, insert the files
    (loop for path in (bowser:get-file-paths dir :max-depth max-depth)
          do (insert padding path "\n"))))

(defun bowser:get-level (str)
  "Gets the indentation level from a string."
  (loop for counter = 0 then (1+ counter)
        for padding = "" then (concat padding bowser:*level-padding*)
        while (string-prefix-p padding str)
        finally (return (1- counter))))

(defun bowser:get-current-line ()
  "Gets the current line."
  (bowser:strip-newline-from-string (thing-at-point 'line)))

(defun bowser:get-level-current-line ()
  "Gets the level of indentation at the current line.  The
padding string is defined by the `bowser:*level-padding*'
variable."
  (bowser:get-level (bowser:get-current-line)))

(defun bowser:get-level-next-line ()
  (if (>= (line-number-at-pos) (count-lines (point-min) (point-max)))
      0
    (save-excursion
      (next-line)
      (bowser:get-level (bowser:get-current-line)))))

(defun bowser:get-path-current-line ()
  "Gets the path at the current line."
  (string-trim (bowser:get-current-line)))

(defun bowser:delete-consecutive-lines (level)
  "Deletes consecutive lines whose level is greater than or equal
to a level."
  (while (and (>= (bowser:get-level-current-line) level)
              (not (= (point) (point-max))))
    (delete-region (point-at-bol) (1+ (point-at-eol)))))

(defun bowser:expand-dir-here ()
  "Expands the dir at the current line."
  (interactive)
  (let ((path (bowser:get-path-current-line))
        (child-level (1+ (bowser:get-level-current-line))))
    (save-excursion
      (bowser:next-line)
      (bowser:delete-consecutive-lines child-level)
      (bowser:insert-paths path :level child-level))
    (bowser:record-dir-expanded path t)))

(defun bowser:collapse-dir-here ()
  "Collapses the dir at the current line."
  (interactive)
  (let ((path (bowser:get-path-current-line))
        (child-level (1+ (bowser:get-level-current-line))))
    (save-excursion
      (bowser:next-line)
      (bowser:delete-consecutive-lines child-level))
    (bowser:record-dir-expanded path nil)))

(defun* bowser:expand-or-collapse-dir (&optional (line (bowser:get-current-line)))
  "Expands or collapses the directory and returns the path to the
directory.  The action (expand/collapse) depends on whether the
directory has been expanded before in the current buffer.
Inspect local variable `local/bowser:dirs-expanded?' for further
info.  If the line does not correspond to a directory path, does
nothing and returns `nil'. "
  (interactive)
  (let* ((path (string-trim line)))
    (when (f-dir? path)
      (let ((path (file-name-as-directory path)))
       (save-excursion
         (if (bowser:is-dir-expanded? path)
             (bowser:collapse-dir-here)
           (bowser:expand-dir-here))))
      path)))

(defun bowser:perform-action-here ()
  "Expands the current directory or opens a file at the current line."
  (interactive)
  (let ((current-point (or (~get-cursor-pos-at-last-mouse-event)
                           (point))))
    (save-excursion
      (goto-char current-point)
      (let* ((line (bowser:get-current-line)))
        (unless (bowser:expand-or-collapse-dir)
          (~execute line))))))

(defun* bowser:browse-dir (&optional (dir (~current-project-root)))
  "Browses a directory."
  (interactive)
  (let* ((buffer-name (bowser:format-buffer-name dir))
         (buffer-exists? (not (null (get-buffer buffer-name))))
         (current-buffer (get-buffer-create buffer-name))
         (starting-pos (point-min)))
    (unless buffer-exists?
      (with-current-buffer current-buffer
        (erase-buffer)

        (insert dir "\n")
        (goto-char (point-min))
        (bowser:perform-action-here)
        ;; (bowser:insert-path dir :level 0)

        (goto-char starting-pos)
        (evil-normal-state)
        (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer)

        (evil-define-key 'normal 'local (kbd "<mouse-2>") #'bowser:perform-action-here)
        (evil-define-key 'insert 'local (kbd "<mouse-2>") #'bowser:perform-action-here)

        (evil-define-key 'insert 'local (kbd "<S-return>") #'bowser:perform-action-here)
        (evil-define-key 'normal 'local (kbd "<S-return>") #'bowser:perform-action-here)))
    (switch-to-buffer current-buffer)))
;; (bowser:browse-dir "/home/hdn/")

(defun bowser:show-command-runner-with-dedicated-frame ()
  "Shows command runner frame.  After running a command, the
frame closes itself."
  (interactive)
  (with-current-buffer (bowser:show-command-runner)
    (setq-local local/delete-frame-on-close (selected-frame))
    (evil-define-key 'insert 'local (kbd "<S-return>") #'bowser:run-text-from-context-then-delete-frame)
    (evil-define-key 'normal 'local (kbd "<S-return>") #'bowser:run-text-from-context-then-delete-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Bowser the directory browser")

(provide 'rmacs:config-module-bowser)
