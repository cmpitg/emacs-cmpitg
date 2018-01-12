;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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
;; Menu
;;

(defun ~right-click-menu ()
  "Returns a list to build a context menu."
  `(""
    ["Cut" clipboard-kill-region (~is-selecting?)]
    ["Copy" kill-ring-save (~is-selecting?)]
    ["Paste" yank t]
    ["Delete" delete-region (~is-selecting?)]
    ["--" ignore]
    ["Exec (other window)" ~exec-in-other-window (~is-selecting?)]
    ["Exec in Tmux" emamux:send-region (~is-selecting?)]
    ["--" ignore]
    ["Undo" undo-tree-undo t]
    ["Redo" undo-tree-redo t]
    ["--" ignore]))

;;
;; Region: https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Region.html
;;

(defalias '~is-selecting? 'use-region-p
  "Determines if current there is a selection/active region.")
(defalias '~selection-start 'region-beginning)
(defalias '~selection-end 'region-end)

(defun ~current-selection ()
  "The currently selected text."
  (if (~is-selecting?)
    (buffer-substring (~selection-start)
                      (~selection-end))
    ""))

(defun ~get-secondary-selection ()
  "Gets the secondary selection (activated with `M-Mouse-1' by
default)."
  (interactive)
  (x-get-selection 'SECONDARY))

;;
;; Editing
;;

(defun ~join-with-next-line ()
  "Joins next line with the current line.  This is just a
convenient wrapper of `join-line'."
  (interactive)
  (join-line -1))

(defun ~open-line (arg)
  "Opens line and moves to the next line."
  (interactive "p")
  (end-of-line)
  (delete-horizontal-space)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun ~open-line-before (arg)
  "Opens line and moves to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

;; TODO cleanup
(defun ~duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.

With argument N, make N copies.
With negative N, comment out original line and use the absolute value.

Source: http://stackoverflow.com/a/4717026/219881"
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next
                                        ; line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not
                                        ; specified
          (insert text))))
    (if use-region nil              ; Only if we're working with a line (not a
                                        ; region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                     ; Comment out original with negative
                                        ; arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun ~move-to-beginning-of-line ()
  "Moves point back to indentation of beginning of line.

Moves point to the first non-whitespace character on this line.
If point is already there, moves to the beginning of the line."
  (interactive)
  (let ((orig-point (point)))
    (unless visual-line-mode
      (back-to-indentation))
    (when (= orig-point (point))
      (beginning-of-visual-line nil))))

(defun* ~previous-line+ (&optional (n-lines 5))
  "Scrolls up `n-lines'."
  (interactive)
  (previous-line n-lines))

(defun* ~next-line+ (&optional (n-lines 5))
  "Scrolls down `n-lines'."
  (interactive)
  (next-line n-lines))

(defun ~mark-line ()
  "Marks current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line))

(defun* ~file-pattern? (str &key (must-exists t))
  "Determines if a string is a file pattern \(`path' or
`path:line-number', or `path:pattern'\).  By default, the
corresponding file must exist for this function to return `t'.
To remove this constraint, pass in `:must-exists nil'.  E.g.

\(~file-pattern? \"/tmp/aoeu\"\)                                        ⇒ t
\(~file-pattern? \"/tmp/aoeu:10\"\)                                     ⇒ t
\(~file-pattern? \"/tmp/aoeu:/hello world/\"\)                          ⇒ t
\(~file-pattern? \"/tmp/non-existent\"\)                                ⇒ nil
\(~file-pattern? \"/tmp/non-existent\" :must-exists nil\)               ⇒ t
\(~file-pattern? \"/tmp/non-existent:10\" :must-exists nil\)            ⇒ t
\(~file-pattern? \"/tmp/non-existent:/hello world/\" :must-exists nil\) ⇒ t
"
  (cl-flet ((check-file-exists? (path) (if must-exists
                                           (f-exists? path)
                                         t)))
    (let ((str (s-trim str)))
      (or (check-file-exists? str)
          (let ((components (s-split ":" str)))
            (and (= 2 (length components))
                 (check-file-exists? (first components))))))))

(defun ~deconstruct-path (path)
  "Deconstructs a path notation into `path' and `number' or
`pattern'.  See the following examples for further information:

\(~deconstruct-path \"/tmp/aoeu\"\)                       ⇒ \(values \"/tmp/aoeu\"\)
\(~deconstruct-path \"/tmp/aoeu:10\"\)                    ⇒ \(values \"/tmp/aoeu\" 10\)
\(~deconstruct-path \"/tmp/aoeu:10/other/:20\"\)          ⇒ \(values \"/tmp/aoeu:10/other/:20\" 20\)
\(~deconstruct-path \"/tmp/aoeu:/hello world/\"\)         ⇒ \(values \"/tmp/aoeu\" \"hello world\"\)
\(~deconstruct-path \"/tmp/aoeu:/inside:/hello world/\"\) ⇒ \(values \"/tmp/aoeu:/inside\" \"hello world\"\)
"
  (let ((matches (or (s-match (rx (group (one-or-more any))
                                  ":" (group (one-or-more digit))
                                  eol)
                              path)
                     (s-match (rx (group (one-or-more any))
                                  ":/" (group (one-or-more any)) "/" eol)
                              path))))
    (if matches
        (let* ((path (nth 1 matches))
               (pattern-or-number (nth 2 matches))
               (number (string-to-int pattern-or-number)))
          (if (zerop number)
              (values path pattern-or-number)
            (values path number)))
      (values path))))

(defun ~insert-full-line-comment ()
  "Inserts a line full of comment characters until `fill-column' is reached."
  (interactive)
  (let ((comment (s-trim comment-start)))
    (thread-first (loop for time from (current-column) upto (1- fill-column) by (length comment)
                        collect comment)
      (string-join "")
      insert)))

(defun ~keyboard-quit ()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we
clicked away or set the cursor into another buffer) we can quit
by pressing 'ESC' three times. This function handles it more
conveniently, as it checks for the condition of not beign in the
minibuffer but having it active. Otherwise simply doing the ESC
or (keyboard-escape-quit) would brake whatever split of windows
we might have in the frame."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (if (or mark-active (active-minibuffer-window))
          (keyboard-escape-quit))
    (keyboard-quit)))

;;
;; Window
;;

(defun ~one-window ()
  "Deletes all other non-dedicated windows and makes current
window the only window visible.  This function does nothing if
the current window is a dedicated window."
  (interactive)
  (unless (window-dedicated-p)
    (mapcar #'(lambda (window)
                (unless (window-dedicated-p window)
                  (delete-window window)))
            (cdr (window-list)))))

(defun ~delete-window ()
  "Deletes current window if it's not sticky/dedicated.  Use
prefix arg (`C-u') to force deletion if it is."
  (interactive)
  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Window '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))
      (delete-window (selected-window))))

;;
;; File & buffer
;;

(defvar *recently-closed-file-list* (list)
  "List of recently closed files.")

(defun ~track-closed-file ()
  "Tracks the list of recently closed files."
  (when-let (path buffer-file-name)
    (delete path *recently-closed-file-list*)
    (add-to-list '*recently-closed-file-list* path)))

(defun ~undo-killed-buffers ()
  "Undoes the kill of buffers."
  (interactive)
  (find-file (completing-read "File: " *recently-closed-file-list*)))

(defalias '~switch-buffer 'ivy-switch-buffer
  "Switches to a buffer and focus the corresponding window & frame.")

(defun ~get-buffer-content (buffer-or-name)
  "Gets the content of a buffer."
  (with-current-buffer buffer-or-name
    (buffer-string)))

(defun ~write-to-file (filename content)
  "Writes to a file."
  (with-temp-buffer
    (insert content)
    (write-file filename)))

(defalias '~switch-to-last-buffer 'mode-line-other-buffer
  "Switches to the most recently visited buffer.")

(defun ~switch-to-scratch ()
  "Switches to `scratch.el' in `*scratch-dir*' directory in
another window."
  (interactive)
  (let ((scratch-dir (or *scratch-dir* temporary-file-directory)))
    (unless (string= "scratch.el"
                     (file-name-nondirectory (or buffer-file-name "")))
      (if (get-buffer "scratch.el")
          (switch-to-buffer-other-window "scratch.el")
        (find-file-other-window (s-concat scratch-dir "scratch.el"))))))

(defun* ~switch-to-messages-buffer (&key (in-other-window t))
  "Switches to the `*Messages*' buffer."
  (interactive)
  (if in-other-window
      (switch-to-buffer-other-window "*Messages*")
    (switch-to-buffer "*Messages*")))

(defun* ~popup-message (content &key (buffer-name "*Temporary*"))
  "Displays a popup window with `content' as its content and an
optional `buffer-name' name.

E.g.

;; Display \"Hello World\" in a popup window.
\(~popup-message \"Hello World\"\)

;; Display \"Hola Mundo\" in a popup window, naming that window buffer \"*mundo*\"
\(~popup-message \"Hello World\" :buffer-name \"*mundo*\"\)
"
  (with-output-to-temp-buffer buffer-name
    (princ content)))

(defalias '~popup-buffer 'internal-temp-output-buffer-show
  "Pops up a buffer for temporary display.")

(defun ~new-buffer ()
  "Opens a new empty buffer in `*scratch-dir*'.  The
corresponding file name for the buffer is set to the current time
and a UUID.  The buffer is save-able and will be deleted upon
exiting unless the local variable `local/delete-on-exit' is set
to `nil'."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq default-directory (or *scratch-dir* temporary-file-directory))
    (set-visited-file-name (thread-first "%s_%s"
                             (format (format-time-string "%Y-%m-%d_%H-%M-%S") (~exec "uuidgen"))
                             string-trim))
    (setq-local local/delete-on-exit t)
    (add-file-local-variable 'local/delete-on-exit t)
    (beginning-of-buffer)
    (setq buffer-offer-save t)))

(defun* ~smart-open-file (path &key (new-frame? nil))
  "Opens path and with external program if necessary."
  (dolist (regexp&action (append (if (boundp '*open-with-regexps*)
                                     *open-with-regexps*
                                   (list))
                                 (list `(".*" . (lambda (path)
                                                  (~open-file-specialized path
                                                                          :new-frame? ,new-frame?))))))
    (let ((regexp (car regexp&action))
          (action (cdr regexp&action)))
      (when (s-matches-p regexp path)
        (return (typecase action
                  (function   (funcall action path))
                  (string     (~open-with path action))
                  (otherwise  (message-box (format "Invalid program %s" action)))))))))

(defun ~gui/open-file ()
  (interactive)
  (let ((path (thread-first "zenity --file-selection --multiple --filename=%s 2>/dev/null"
                (format (thread-first default-directory
                          file-name-as-directory
                          shell-quote-argument))
                ~exec
                string-trim)))
    (unless (string-empty-p path)
      (find-file path))))

(defun ~gui/save-as ()
  (interactive)
  (let ((path (thread-first "zenity --file-selection --save --confirm-overwrite --filename=%s 2>/dev/null"
                (format (thread-first default-directory
                          file-name-as-directory
                          shell-quote-argument))
                ~exec
                string-trim)))
    (unless (string-empty-p path)
      (write-file path nil))))

(defun ~web-browse-gui (uri)
  "Calls a GUI browser on a URI."
  (interactive "MURI: ")
  (~open-with uri "web-browser-gui %s"))

(defun ~open-with (path program)
  "Opens path with `program'.  The `path' path is quoted
automatically."
  (~run-process (format program (shell-quote-argument path))))

(defun ~find-file-new-frame (path &optional wildcards)
  "Calls `find-file' in a new frame."
  (let ((frame (make-frame)))
    (select-frame frame)
    (find-file path wildcards)))

(defun* ~open-file-specialized (file-pattern &key (new-frame? nil))
  "Opens a path and jumps to a line based on number or a the
first occurrence of a pattern.  E.g.

* If `file-pattern' is a path, open it;

* If `file-pattern' is of format \"<path>:<number>\", open the
  file and jump to the corresponding line number;

* If `file-pattern' is of format \"<path>:/<pattern>/\", open the
  file and jump to the first occurrence of `pattern'.
"
  (multiple-value-bind (path pattern)
      (~deconstruct-path file-pattern)
    (if new-frame?
        (~find-file-new-frame path)
      (find-file path))
    (when pattern
      (cond ((numberp pattern)
             (goto-line pattern))
            (t
             (beginning-of-buffer)
             (re-search-forward pattern))))
    path))

(defun ~delete-current-file ()
  "Deletes the file associated with the current buffer and kills
off the buffer."
  (interactive)
  (let ((current-file buffer-file-name))
    (when (and (file-exists-p current-file)
               (yes-or-no-p (concat "Delete file: " current-file)))
      ;; Prevent the following kill-buffer from recursively calling this
      ;; function
      (when (local-variable-p 'local/delete-on-exit)
        (kill-local-variable 'local/delete-on-exit))
      (kill-buffer (current-buffer))

      (delete-file current-file)
      (message "%s deleted" current-file))))

(defun ~maybe-delete-file-when-killing-buffer ()
  "Deletes current file when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-on-exit)
             local/delete-on-exit)
    (~delete-current-file)))

(defun ~maybe-make-current-file-executable ()
  "Checks for the hashbang and `chmod u+x`s current file if
needed."
  (interactive)
  (and
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (save-match-data
         (looking-at "^#!/"))))
   (not (file-executable-p buffer-file-name))
   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
   (revert-buffer)
   (message "%s saved as executable" buffer-file-name)))

(defun ~clean-up-tramp ()
  "Closes all tramp connections and buffers."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(defun ~counsel-ag-default-project-root ()
  "Calls `counsel-ag', taking project root by default and
fallback to current directory if project root is not found."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'counsel-ag)
    (counsel-ag nil (or (ignore-errors (projectile-project-root))
                        default-directory))))

;;
;; Emacs Lisp
;;

(defun ~eval-string (str)
  "Evals a string."
  (interactive "sString: ")
  (eval (first (read-from-string (concat "(progn " str ")")))))

(defun* ~eval-region ()
  "Evals region and returns value."
  (interactive)
  (if (~is-selecting?)
      (thread-first (buffer-substring (region-beginning) (region-end))
        ~eval-string)
    (error "No region to eval")))

(defun ~eval-current-sexp ()
  "Evals the current enclosing sexp."
  (interactive)
  (let ((current-point (point)))
    (call-interactively 'er/mark-outside-pairs)
    (let ((res (call-interactively '~eval-region)))
      (prin1 res)
      (goto-char current-point)
      (setq deactivate-mark t)
      res)))

(defun ~eval-last-sexp-or-region ()
  "Evals region if active, or evals last sexpr."
  (interactive)
  (if (~is-selecting?)
      (call-interactively '~eval-region)
    (call-interactively 'eval-last-sexp)))

(defun ~eval-then-replace-region-or-last-sexp ()
  "Evals then replaces region or last sexp with result."
  (interactive)
  (let ((value (~eval-last-sexp-or-region)))
    (if (~is-selecting?)
        (delete-region (region-beginning) (region-end))
      (kill-sexp -1))
    (insert (format "%s" value))))

(defun ~read-command-or-get-from-secondary-selection ()
  "Without prefix argument, if there is an active selection,
returns it (assuming that it denotes a shell command); otherwise,
reads and returns a shell command from the minibuffer.

With prefix argument, always reads the shell command from the
minibuffer."
  (interactive)
  (if (and (~get-secondary-selection) (not current-prefix-arg))
      (~get-secondary-selection)
    (read-shell-command "Command: ")))

;;
;; Processes
;;

(defun ~get-process-output (process)
  "Gets the output for a managed process."
  (with-current-buffer (process-buffer process)
    (buffer-string)))

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

(defun* ~exec (command &key (stdin nil))
  "Executes a shell command then returns its value as string.
The `stdin' parameter determines where to read stdin or the shell
command, if `stdin' is:
* `nil' → no stdin is taken into account;
* `:region' → stdin is taken from the current region;
* any other value → takes the value of `stdin' as stdin of the shell
  command."
  (interactive "MCommand: ")
  (pcase stdin
    ('nil (with-temp-buffer
            (shell-command command t nil)
            (buffer-string)))
    (:region (let ((inhibit-message t))
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
    (stdin-value (let ((inhibit-message t))
                   (with-temp-buffer
                     (insert stdin-value)
                     (shell-command-on-region (point-min)
                                              (point-max)
                                              command

                                              ;; Output buffer name
                                              command

                                              ;; Don't replace current region
                                              nil

                                              ;; Error piped to output
                                              nil)))
                 (~get-buffer-content command))))

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

(message "Finished configuring core functions")

(provide 'rmacs:config-core-functions)
