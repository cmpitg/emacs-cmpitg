;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2018-2020 Ha-Duong Nguyen (@cmpitg)
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

;; TODO: Check: async-start-process → ~exec-|-async

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-context-menu ()
  "Returns a list to build a context menu."
  `(""
    ["Switch to buffer" ~switch-buffer]
    ["Buffer list" list-buffers]
    ["--" ignore]
    ["Cut" clipboard-kill-region (~is-selecting?)]
    ["Copy" kill-ring-save (~is-selecting?)]
    ["Paste" ,#'(lambda ()
                  (interactive)
                  (when (~is-selecting?)
                    (call-interactively #'delete-region))
                  (call-interactively #'yank))]
    ["Clone" ~duplicate-line-or-region]
    ["Delete" delete-region (~is-selecting?)]
    ["--" ignore]
    ["Exec" ~execute]
    ["Exec kill buffer" ,#'(lambda ()
                             (interactive)
                             (call-interactively #'~execute)
                             (call-interactively #'kill-current-buffer))]
    ["Exec line" ~execute-line]
    ["Exec line and kill buffer" ,#'(lambda ()
                                      (interactive)
                                      (call-interactively #'~execute-line)
                                      (call-interactively #'kill-current-buffer))]
    ["--" ignore]
    ["Eval last sexp or region" ~eval-last-sexp-or-region]
    ;; TODO: Include buffer list, new temp buffer, ...
    ["--" ignore]
    ["Horizontal split" split-window-horizontally]
    ["Vertical split" split-window-vertically]
    ["One window" ~one-window]
    ["Delete current window" delete-window]
    ["--" ignore]
    ["Undo" undo-tree-undo t]
    ["Redo" undo-tree-redo t]
    ["--" ignore]))

(defun ~popup-context-menu ()
  "Pops up the context menu."
  (interactive)
  (popup-menu (~get-context-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level functions for better UX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~split-window (&optional (side :right))
  "Splits the current window & switch to the new window."
  (interactive)
  (when-let (window (split-window (selected-window) nil side nil))
    (select-window window)
    (call-interactively #'~switch-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Region: https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Region.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '~is-selecting? 'use-region-p
  "Determines if current there is a selection/active region.")

(defun ~get-selection ()
  "Gets the currently selected text."
  (if (~is-selecting?)
    (buffer-substring (region-beginning) (region-end))
    ""))

(defun ~get-secondary-selection ()
  "Gets the secondary selection (activated with `M-Mouse-1' by
default)."
  (interactive)
  (gui-get-selection 'SECONDARY))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (forward-line 1)
  (indent-according-to-mode))

(defun ~open-line-before (arg)
  "Opens line and moves to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

;; TODO: To implement properly: lines should be selected regardless of where the cursor is?
(defun ~select-line ()
  "Selects a line."
  (interactive)
  (beginning-of-line)
  (call-interactively #'set-mark-command)
  (end-of-line))

(defun ~kill-whole-line ()
  "Kills the whole line."
  (interactive)
  (beginning-of-line)
  (kill-line 1))

;; BUG: Doesn't work with subwords
(defun ~forward-word-boundary ()
  "Forwards to the next word boundary."
  (interactive)
  (destructuring-bind (_ . word-end-pos)
      (if (bounds-of-thing-at-point 'word)
          (bounds-of-thing-at-point 'word)
        (cons nil nil))
    (forward-word)
    (when (and (not (null word-end-pos))
               (< word-end-pos (point)))
      (backward-word))))

;; BUG: Doesn't work with subwords
(defun ~backward-word-boundary ()
  "Backwards to the previous word boundary."
  (interactive)
  (destructuring-bind (word-start-pos . _)
      (if (bounds-of-thing-at-point 'word)
          (bounds-of-thing-at-point 'word)
        (cons nil nil))
    (backward-word)
    (when (and (not (null word-start-pos))
               (< (point) word-start-pos))
      (forward-word))))

(defun ~select-word ()
  "Selects current word."
  (interactive)
  (when (bounds-of-thing-at-point 'word)
    (forward-word)
    (call-interactively #'set-mark-command)
    (backward-word)))

(defun ~delete-blank-lines ()
  "Deletes all blank lines at the current position."
  (interactive)
  (delete-blank-lines)
  (when (looking-at (rx bol (0+ space) eol))
    (kill-line)))

(defun ~delete-output-block ()
  "Removes the current output block."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (call-interactively '~mark-current-output-block)
    (delete-region (region-beginning) (region-end))))

(cl-defun ~search-buffer-interactively ()
  "Searches the current buffer interactively."
  (interactive)
  (swiper (~get-selection)))

(cl-defun ~show-buffer-chooser ()
  "Shows the buffer chooser tool."
  (interactive)
  (require 'rmacs:config-module-simple-buffer-list)
  (call-interactively #'bl:show-buffer-chooser))

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
        (dotimes (_ (abs (or n 1)))     ; Insert N times, or once if not
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
  "Moves the point to the first non-whitespace character on this line.
If the point is already there, moves to the beginning of the
line."
  (interactive)
  (let ((orig-point (point)))
    (unless visual-line-mode
      (back-to-indentation))
    (when (= orig-point (point))
      (beginning-of-visual-line nil))))

(cl-defun ~previous-line+ (&optional (n-lines 5))
  "Scrolls up `n-lines'."
  (interactive)
  (forward-line (- n-lines)))

(cl-defun ~next-line+ (&optional (n-lines 5))
  "Scrolls down `n-lines'."
  (interactive)
  (forward-line n-lines))

(cl-defun ~file-pattern? (str &key (must-exists t))
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
               (number (string-to-number pattern-or-number)))
          (if (zerop number)
              (values path pattern-or-number)
            (values path number)))
      (values path))))

(defun ~insert-full-line-comment ()
  "Inserts a line full of comment characters until `fill-column'
is reached."
  (interactive)
  (let ((comment (s-trim comment-start)))
    (thread-first
        (loop for time from (current-column) upto (1- fill-column) by (length comment)
              collect comment)
      (string-join "")
      insert)))

(cl-defun ~insert-file-contents-and-goto-end (filepath)
  "Inserts the contents of a file and go to the end of the
content in buffer."
  (lexical-let ((current-pos (point)))
    (destructuring-bind (_ n-chars) (insert-file-contents filepath)
      (goto-char (+ n-chars current-pos)))))

(cl-defun ~insert-output-block (content
                                &key replace-current-block?
                                print-output-marker?
                                (colorize-with :overlay))
  "Inserts an output block.

If REPLACE-CURRENT-BLOCK? is t, try replacing the current output
block.

If PRINT-OUTPUT-MARKER? is t, print also the output
markers (defined by *~OUTPUT-BEGINNING-MARKER* and
*~OUTPUT-END-MARKER*).

COLORIZE-WITH is one of the following keywords:
· :overlay - the content is ANSI-colorized using overlays
· :text-properties - the content is ANSI-colorized using text properties
Invalid values has no effects."
  (when replace-current-block?
    (call-interactively #'~delete-output-block))

  ;; Deliberately use setq here for readability.  let-only bindings look ugly.
  (let (start-point
        end-point
        (content (if (eq :text-property colorize-with)
                     (~ansi-colorize content)
                   content)))
    (when print-output-marker? (insert *~output-beginning-marker* "\n"))
    (setq start-point (point))
    (insert content)
    (setq end-point (point))
    (when print-output-marker? (insert "\n" *~output-end-marker*))

    (when (eq :overlay colorize-with) (ansi-color-apply-on-region start-point end-point))))

(defun ~keyboard-quit ()
  "Escapes the minibuffer or cancels region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we
clicked away or set the cursor into another buffer) we can quit
by pressing 'ESC' three times.  This function handles it more
conveniently, as it checks for the condition of not beign in the
minibuffer but having it active.  Otherwise simply doing the ESC
or (keyboard-escape-quit) would brake whatever split of windows
we might have in the frame."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (if (or mark-active (active-minibuffer-window))
          (keyboard-escape-quit))
    (keyboard-quit)))

(defun ~insert-exec ()
  "Inserts an executable from PATH or the current working
directory to the current buffer."
  (interactive)
  ;; TODO: Save history
  (defvar *~recent-inserted-execs* (list)
    "List of recently inserted executables.")
  (let* ((execs (s-lines (~exec-sh "get-all-execs")))
         (current-dir-execs (thread-first (~exec-sh (list "find" "." "-type" "f" "-maxdepth" "1"))
                              (s-lines)
                              (butlast)))
         (all-execs (append *~recent-inserted-execs* current-dir-execs execs))
         (output (ivy-read "Choose exec: " all-execs
                           :history '*~recent-inserted-execs*)))
    (unless (s-blank? output)
      (insert output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window & Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-frames-mru ()
  "Gets the list of frames is most-recently-used order."
  (sort (frame-list)
        #'(lambda (x y)
            (> (or (frame-parameter x :custom/focus-time) 0)
               (or (frame-parameter y :custom/focus-time) 0)))))

(defun ~get-previous-frame ()
  "Gets the previously focused frame that is not the current one."
  (if (= 1 (length (frame-list)))
      nil
    (cl-loop for frame in (~get-frames-mru)
             unless (equalp frame (selected-frame))
             return frame)))

(defun ~toggle-sticky-window ()
  "Toggles stickiness of the currently active window."
  (interactive)
  (let* ((window (get-buffer-window (current-buffer)))
         (sticky? (window-dedicated-p window)))
    (set-window-dedicated-p window (not sticky?))
    (message (if (not sticky?)
                 "Window '%s' is now sticky"
               "Window '%s' is now normal")
             (current-buffer))))

(cl-defun ~toggle-toolbox (&key (path *toolbox-path*)
                              (side 'right)
                              (size -78)
                              (follow-dir t))
  "Toggles toolbox file.  The path to the toolbox file is passed
on using the `path' argument.  The toolbox window is sticky,
appears on the `side', and using `size' as its width.
`follow-dir' determines whether or not the toolbox buffer
inherits the working directory from the buffer that calls it.
Returns the toolbox window."
  (interactive)
  ;; TODO: Correctly get the buffer
  (let ((toolbox-buffer (get-file-buffer path))
        (working-dir default-directory))
    (if-let (window (and toolbox-buffer
                         (get-buffer-window toolbox-buffer)))
        (delete-window window)
      (progn
        (split-window (selected-window) size side)
        (with-selected-window (windmove-do-window-select side)
          (let ((split-window-preferred-function nil)
                (pop-up-windows nil))
            (~smart-open-file path))
          (when follow-dir (setq-local default-directory working-dir))
          (set-window-dedicated-p (selected-window) t)
          (selected-window))))))

(defun ~toggle-maximize-buffer ()
  "Toggles maximizing current buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(cl-defun ~scroll-other-window (&key (nlines 5))
  "Scrolls the other window."
  (interactive)
  (scroll-other-window nlines))

(cl-defun ~scroll-other-window-reverse (&key (nlines 5))
  "Scrolls the other window in reverse direction."
  (interactive)
  (scroll-other-window (- nlines)))

(cl-defun ~kill-buffer-and-frame (&optional (buffer (current-buffer)))
  "Kills the a buffer along with its frame (if exists)."
  (interactive)
  (unless (null buffer)
    (if-let (window (get-buffer-window buffer t))
        (let ((frame (window-frame window)))
          (kill-buffer buffer)
          (delete-frame frame))
      (kill-buffer buffer))))

(cl-defun ~kill-buffer-and-window (&optional (window (selected-window)))
  "Kills the a buffer along with its window (if exists)."
  (interactive)
  (with-selected-window window
    (if (= (~count-non-sticky-windows) 1)
        (kill-buffer)
      (kill-buffer-and-window))))

(cl-defun ~count-non-sticky-windows ()
  "Counts the number of non-sticky windows in the current frame."
  (loop for window being the windows
        unless (window-dedicated-p window)
        count window))

(cl-defun ~count-windows ()
  "Counts the number of windows in the current frame."
  (loop for window being the windows
        count window))

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
      (call-interactively #'delete-window)))

(cl-defun ~get-next-non-dedicated-window (&optional original-window next-window)
  "Gets the next non-dedicated, non-minibuffer window."
  (cond
   ((equal original-window next-window)
    original-window)
   ((null next-window)
    (~get-next-non-dedicated-window original-window (next-window original-window)))
   ((window-dedicated-p next-window)
    (~get-next-non-dedicated-window original-window (next-window next-window)))
   (t
    next-window)))

(cl-defun ~get-non-minibuffer-window-in-dir (dir)
  "Gets the next non-minibuffer in a direction.
If the found window is the mini-buffer, returns `nil'."
  (require 'windmove)
  (let ((window (windmove-find-other-window dir)))
    (unless (minibufferp (window-buffer window))
      window)))

(cl-defun ~transpose-windows (&optional (window-selection-fn #'~get-next-non-dedicated-window))
  "Transposes the current window with the next one."
  (interactive)
  (when (window-dedicated-p (selected-window))
    (error "Current window is dedicated, cannot transpose"))
  (let ((windows (loop for window in (window-list)
                       when (window-dedicated-p window)
                       collect window)))
    (let* ((current-window (selected-window))
           (current-buffer (window-buffer))
           (next-window (apply window-selection-fn current-window nil))
           (next-buffer (window-buffer next-window)))
      (set-window-buffer next-window current-buffer)
      (set-window-buffer current-window next-buffer))))

(defun ~set-pop-up-buffer-mode (mode)
  "Sets mode for pop-up buffer.  MODE should either be :WINDOW or :FRAME."
  (cl-case mode
    (:window
     (custom-set-variables `(*popup-buffer-in* :window)
                           `(display-buffer-alist nil))

     ;; Prefer creating a new window
     ;; (custom-set-variables `(pop-up-windows t))

     ;; Prefer reusing the current window
     (custom-set-variables `(pop-up-windows nil)))
    (:frame
     (custom-set-variables `(*popup-buffer-in* :frame))

     ;; To make the behavior of `display-buffer' consistent, do not allow it
     ;; to split/create a new window by setting to `nil'
     (setq pop-up-windows nil)

     ;; Display some buffers in a separate frame so that they don't steal the
     ;; current window
     (dolist (buffer-regex (list (rx bol "*Help*" eol)
                                 (rx bol "*compilation*" eol)
                                 (rx bol "*cider-result*" eol)
                                 (rx bol "*cider-doc*" eol)
                                 (rx bol "*Org Agenda*" eol)
                                 (rx bol "*ivy-occur")))
       (add-to-list 'display-buffer-alist (cons buffer-regex (cons #'special-display-popup-frame nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File & buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Make the tracking of recently closed file a separate module
(defun ~track-closed-file ()
  "Tracks the list of recently closed files."
  (defvar *recently-closed-file-list* (list)
    "List of recently closed files.")
  (when-let (path buffer-file-name)
    (delete path *recently-closed-file-list*)
    (add-to-list '*recently-closed-file-list* path)))

(defun ~undo-killed-buffers ()
  "Undoes the kill of buffers."
  (interactive)
  (defvar *recently-closed-file-list* (list)
    "List of recently closed files.")
  (find-file (let ((ivy-sort-functions-alist nil))
               (completing-read "File: " *recently-closed-file-list*))))

;; (defalias '~switch-buffer 'ivy-switch-buffer
;;   "Switches to a buffer and focus the corresponding window & frame.")

(defalias '~switch-buffer '~show-buffer-chooser
  "Switches to a buffer and focus the corresponding window & frame.")

(cl-defun ~clean-up-buffer (&key (buffer (current-buffer))
                               (keep-local-vars? nil))
  "Cleans up buffer."
  (interactive)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless keep-local-vars?
        (kill-all-local-variables))
      (remove-overlays))))

;; FIXME: Review these hippie-expand enhancements
;; (defun try-expand-flexible-abbrev (old)
;;   "Tries to complete word using flexible matching.

;; Flexible matching works by taking the search string and then
;; interspersing it with a regexp for any character. So, if you try
;; to do a flexible match for `foo' it will match the word
;; `findOtherOtter' but also `fixTheBoringOrange' and
;; `ifthisisboringstopreadingnow'.

;; The argument OLD has to be nil the first call of this function, and t
;; for subsequent calls (for further possible completions of the same
;; string).  It returns t if a new completion is found, nil otherwise."
;;   (unless old
;;     (he-init-string (he-lisp-symbol-beg) (point))
;;     (if (not (he-string-member he-search-string he-tried-table))
;;         (setq he-tried-table (cons he-search-string he-tried-table)))
;;     (setq he-expand-list
;;           (and (not (equal he-search-string ""))
;;                (he-flexible-abbrev-collect he-search-string))))

;;   (while (and he-expand-list
;;               (he-string-member (car he-expand-list) he-tried-table))
;;     (setq he-expand-list (cdr he-expand-list)))

;;   (cond ((null he-expand-list)
;;          (if old
;;              (he-reset-string))
;;          nil)
;;         (t
;;          (he-substitute-string (car he-expand-list))
;;          (setq he-expand-list (cdr he-expand-list))
;;          t)))

;; (defun he-flexible-abbrev-collect (str)
;;   "Find and collect all words that flex-matches STR.
;; See docstring for `try-expand-flexible-abbrev' for information
;; about what flexible matching means in this context."
;;   (let ((collection nil)
;;         (regexp (he-flexible-abbrev-create-regexp str)))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp regexp nil t)
;;         ;; Is there a better or quicker way than using `thing-at-point' here?
;;         (setq collection (cons (thing-at-point 'word) collection))))
;;     collection))

;; (defun he-flexible-abbrev-create-regexp (str)
;;   "Generate regexp for flexible matching of STR.
;; See docstring for `try-expand-flexible-abbrev' for information
;; about what flexible matching means in this context."
;;   (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
;;           "\\w*" "\\b"))

(defvar *electrify-return-match*
  "[\]\)]"
  ;; "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an
\"electric\" return.")
(defun ~electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match'
then opens and indents an empty line between the cursor and the
text.  Moves the cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at *electrify-return-match*)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun ~get-last-sexp ()
  "Returns the last sexp before the current point."
  (interactive)
  (elisp--preceding-sexp))

(defun ~get-cursor-pos-at-last-mouse-event ()
  "Returns the position of the mouse cursor if the last command
event is a mouse event, or `nil' otherwise."
  (interactive)
  (posn-point (event-end last-command-event)))

(defun ~try-getting-current-thing ()
  "Returns text from the current context:

- If the last command is a mouse event, go to the point under the
  cursor.

- if the current line is a path, returns it; or

- if the thing-at-point could be retrieved as a symbol, returns
  its string representation; otherwise

- returns the last sexp."
  (interactive)
  (require 'rmacs:config-module-bowser)
  (save-excursion
    (if-let (point (~get-cursor-pos-at-last-mouse-event))
        (goto-char point))
    (let ((path (bowser:get-path-current-line)))
      (or (and (~file-pattern? path) path)
          (thing-at-point 'symbol)
          (~get-last-sexp)))))

(defun ~get-buffer-list ()
  "Gets the current buffer list, ordered by last visited."
  (lexical-let ((res (iflipb-interesting-buffers)))
    (concatenate 'list
                 (iflipb-interesting-buffers)
                 (loop for b in (buffer-list)
                       unless (member b res)
                       collect b))))

(defun ~get-current-buffer-index ()
  "Gets index of the current buffer in the buffer list returned
by `~get-buffer-list'."
  iflipb-current-buffer-index)

(defun ~get-buffer-content (buffer-or-name)
  "Gets the content of a buffer."
  (with-current-buffer buffer-or-name
    (buffer-string)))

(defun ~write-to-file (filename content)
  "Writes to a file."
  (with-temp-buffer
    (insert content)
    (write-file filename)))

(defun ~read-file (path)
  "Reads file and returns content as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ~rename-current-file (&optional new-name)
  "Renames the current file."
  (interactive "GNew name: ")
  (let* ((new-name (expand-file-name new-name))
         (name (buffer-name))
         (filename (buffer-file-name)))
    (if (not filename)
        (error "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defalias '~switch-to-last-buffer 'mode-line-other-buffer
  "Switches to the most recently visited buffer.")

(defun ~toggle-scratch ()
  "Toggles the `scratch.el' buffer.  `scratch.el' should reside
in the `*scratch-dir*' directory."
  (interactive)
  (let* ((scratch-dir (or *scratch-dir* temporary-file-directory))
         (scratch-file (s-concat scratch-dir "scratch.el")))
    (~toggle-toolbox :path scratch-file :size -80)))

(cl-defun ~switch-to-messages-buffer (&key (in-other-window nil))
  "Switches to the `*Messages*' buffer."
  (interactive)
  (if in-other-window
      (switch-to-buffer-other-window "*Messages*")
    (switch-to-buffer "*Messages*")))

(defun ~unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line
of text.  See https://www.emacswiki.org/emacs/UnfillParagraph for
reference."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; TODO: Thinking
(cl-defun ~setup-temp-buffer (&optional (buffer (current-buffer)))
  "Sets up a temporary buffer."
  (interactive)
  (with-current-buffer buffer
    (with-eval-after-load "evil"
      (when evil-mode
        (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer)))
    (bind-key "q" #'kill-current-buffer (current-local-map))))

(cl-defun ~popup-buffer-frame (&key (buffer "*Temp*")
                                    content
                                    working-dir)
  "Pops up a buffer in a new frame, useful for workflows with
tiling window manager.  If CONTENT is non-nil, it serves as the
content of the buffer.  When the buffer is closed, the
corresponding frame is deleted."
  (interactive)
  (let* ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (when content
        (~clean-up-buffer)
        (insert content)
        (with-eval-after-load "evil"
          (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer))
        (bind-key "q" #'kill-current-buffer (current-local-map)))

      (switch-to-buffer-other-frame buffer)
      (setq-local default-directory working-dir)
      (setq-local local/delete-frame-on-close (selected-frame)))))

;; TODO: Check if keymap exists
(cl-defun ~popup-buffer-window (&key (buffer "*Temp*")
                                     content
                                     working-dir
                                     (size 80))
  "Pops up a buffer in a new window.  If CONTENT is non-nil,
it serves as the content of the buffer.  SIZE defines the width
threshold which the window receives.  When the buffer is closed,
the corresponding window is deleted."
  (interactive)
  (let ((buffer (get-buffer-create buffer)))
    ;; Make sure the input window doesn't exist in any frame
    (when-let (wind (get-buffer-window buffer t))
      (delete-window wind))

    ;; Now create the window
    (with-current-buffer buffer
      (when content
        (~clean-up-buffer)
        (insert content))

      (split-window (selected-window) size 'left)
      (setq-local default-directory working-dir)
      (setq-local local/delete-window-on-close t)
      (switch-to-buffer buffer))
    buffer))

(cl-defun ~popup-buffer (&key (buffer "*Temp*")
                              content
                              working-dir
                              (size 80))
  "Pops up a buffer in a new window or frame.  If CONTENT is non-nil,
it serves as the content of the buffer.  SIZE defines the width
threshold which the window receives in case a new window is
popped up.  When the buffer is closed, the corresponding
window/frame is deleted.

This function will pop up a buffer window if the variable
*POPUP-BUFFER-IN* is :window and it is called without a prefix
argument.  Otherwise, it will pop up a buffer frame."
  (interactive)
  (cond
   ((or (eq :frame *popup-buffer-in*)
        current-prefix-arg)
    (~popup-buffer-frame :buffer buffer
                         :content content
                         :working-dir working-dir))
   ((eq :window *popup-buffer-in*)
    (~popup-buffer-window :buffer buffer
                          :content content
                          :working-dir working-dir
                          :size size))
   (t
    (error "Unrecognized value of *popup-buffer-in*: %s. It must be either :window or :frame."
           *popup-buffer-in*))))

;; (defalias '~popup-buffer 'internal-temp-output-buffer-show
;;   "Pops up a buffer for temporary display.")

(defun ~new-buffer-frame-from-selection ()
  "Opens a new frame with a temporary buffer containing the
  current selection."
  (interactive)
  (~popup-buffer-frame :content (~get-selection)
                       :working-dir default-directory))

(defun ~new-buffer ()
  "Opens a new empty buffer in *SCRATCH-DIR* and returns it.
The corresponding file name for the buffer is set to the current
time and a UUID.  The buffer is save-able and will be deleted
upon exiting unless the local variable LOCAL/DELETE-ON-CLOSE is
set to nil."
  (interactive)
  (lexical-let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq default-directory (or *scratch-dir* temporary-file-directory))
    (set-visited-file-name (thread-first "%s_%s"
                             (format (format-time-string "%Y-%m-%d_%H-%M-%S") (~gen-filename))
                             string-trim))
    (let ((var/symbol (make-local-variable 'local/delete-on-close)))
      (set var/symbol t)
      (add-file-local-variable var/symbol t))
    (goto-char (point-min))
    (setq buffer-offer-save t)
    buf))

(defun ~new-buffer-associated-with-frame ()
  "Opens a new buffer with `~new-buffer' and associates it with
the current frame, i.e. the frame is deleted when the buffer is
killed.  Returns that buffer."
  (interactive)
  (with-current-buffer (~new-buffer)
    (setq-local local/delete-frame-on-close (selected-frame))
    (current-buffer)))

(defun ~revert-all-file-buffers-no-confirmation ()
  "Reverts all file-backed buffers without confirmation (by
assuming a 'yes' answer).  This function is useful when calling
at the end of Emacs startup stage to make sure configuration
which is loaded lazily get loaded."
  (interactive)
  (loop for buf in (buffer-list)
        for file-name = (buffer-file-name buf)
        when (and file-name (file-exists-p file-name))
        do (ignore-errors (with-current-buffer buf
                            (revert-buffer t t)))))

;; TODO: Remove the 'open with' logic, replacing it with dispatch-action?
(cl-defun ~smart-open-file (path &key (new-frame? nil))
  "Opens path and with external program if necessary.  `path' is
expanded using `expand-file-name', then
`substitute-in-file-name'."
  (let ((path (string-trim (expand-file-name path))))
    (cl-dolist (regexp&action
                (append (if (boundp '*open-with-regexps*)
                            *open-with-regexps*
                          (list))
                        (list `(".*" . (lambda (path)
                                         (~open-file-specialized path
                                                                 :new-frame? ,new-frame?))))))
      (let ((regexp (car regexp&action))
            (action (cdr regexp&action)))
        (when (s-matches-p regexp path)
          (cl-return (typecase action
                       (function   (funcall action path))
                       (string     (~open-with path action))
                       (otherwise  (error "Invalid program %s" action)))))))))

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

(defun ~open-with-google-chrome (uri)
  "Opens a URI with Google Chrome."
  (interactive "MURI: ")
  (~open-with uri "run-chrome %s"))

(defun ~open-with (path program)
  "Opens path with `program'.  The `path' path is quoted
automatically."
  (~run-process (format program (shell-quote-argument path))))

(defun ~find-file-new-frame (path &optional wildcards)
  "Calls `find-file' in a new frame."
  (let ((frame (make-frame)))
    (select-frame frame)
    (find-file path wildcards)))

(defun ~find-file-in-previous-frame (path)
  "Visits a file in the previous frame, or creates a new frame
then visits if there is no previous frame."
  (interactive "GPath: ")
  (when-let (frame (~get-previous-frame))
    (lexical-let ((dir default-directory))
      (select-frame frame)
      (find-file (f-join dir path)))))

(cl-defun ~open-file-specialized (file-pattern &key (new-frame? nil))
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
             (goto-char (point-min))
             (re-search-forward pattern))))
    path))

(defun ~visit-file (path)
  "Visits a file without opening it and returns the buffer name."
  (buffer-name (find-file-noselect path)))

(defun ~open-toolbox ()
  "Opens toolbox file."
  (interactive)
  (~smart-open-file *toolbox-path*))

(defun ~copy-pos-to-clipboard ()
  "Appends path to the current position to the end of window on
the right so that it could be open with `~smart-open-file' later
on."
  (interactive)
  (let* ((selection (thread-last (~get-selection)
                      (s-replace "/" "\\/")))
         (line-number-or-pattern (if (~is-selecting?)
                                     (s-concat "/" selection "/")
                                   (line-number-at-pos)))
         (path (format "%s:%s" (buffer-file-name) line-number-or-pattern)))
    (~copy-to-clipboard path)))

(defun ~copy-file-name-to-clipboard ()
  "Copies current file/dir name to clipboard."
  (interactive)
  (let ((filename (if (eq major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (~copy-to-clipboard filename))))

(defun ~parse-tramp-argument (connection-string)
  "Returns an alist with

* protocol
* username
* host
* port
* path

from a Tramp connection string.

E.g.

\(~parse-tramp-argument \"/ssh:cmpitg@192.168.1.4#3355:/etc/network/interfaces\"\)
;; =>
;; \(\(protocol . \"ssh\"\)
;;  \(username . \"cmpitg\"\)
;;  \(host . \"192.168.1.4\"\)
;;  \(port . \"3355\"\)
;;  \(path . \"/etc/network/interfaces\"\)\)

\(~parse-tramp-argument \"/home/cmpitg/tmp/tmp.txt\"\)
;; =>
;; \(\(protocol . \"\"\)
;;  \(username . \"cmpitg\"\)
;;  \(host . \"localhost\"\)
;;  \(port . \"\"\)
;;  \(path . \"/home/cmpitg/tmp/tmp.txt\"\)\)
"
  (if (not (string-match "@" connection-string))
      `((protocol . "")
        (username . ,user-login-name)
        (host     . "localhost")
        (port     . "")
        (path     . ,connection-string))
    (cl-flet ((get-path (host-and-path)
                        (if (string-match (rx (group "/" (1+ anything))) host-and-path)
                            (match-string 1 host-and-path)
                          "/tmp/"))
              (get-port (host-and-path)
                        (if (string-match (rx (1+ (not (any "\\")))
                                              "#"
                                              (group (1+ digit)))
                                          host-and-path)
                            (match-string 1 host-and-path)
                          "22"))
              (get-host (host-and-path)
                        (if (string-match (rx bol
                                              (group (1+ (not (any "#" ":")))))
                                          host-and-path)
                            (match-string 1 host-and-path)
                          "localhost")))

      (string-match "^/\\([^:]+\\):\\([^@]+\\)@\\(.*\\)$" connection-string)

      (let* ((protocol      (match-string 1 connection-string))
             (username      (match-string 2 connection-string))
             (host-and-path (match-string 3 connection-string))

             (host          (get-host host-and-path))
             (port          (get-port host-and-path))
             (path          (get-path host-and-path)))
        `((protocol . ,protocol)
          (username . ,username)
          (host     . ,host)
          (port     . ,port)
          (path     . ,path))))))

(defun ~open-current-file-as-admin ()
  "Opens the current buffer as *nix root.
This command works on `sudo` *nixes only."
  (interactive)
  (when buffer-file-name
    (let* ((parsed-data (~parse-tramp-argument buffer-file-name))
           (username  (alist-get 'username parsed-data))
           (host      (alist-get 'host parsed-data))
           (path      (alist-get 'path parsed-data))
           (port      (alist-get 'port parsed-data)))
      (find-alternate-file
       (if (string-empty-p port)
           (format "/sudo:root@%s:%s"
                   host
                   path)
         ;; See Tramp's multiple hop
         (progn
           (message (format "/ssh:%s@%s#%s|sudo:%s#%s:%s"
                            username
                            host
                            port
                            host
                            port
                            path))
           (format "/ssh:%s@%s#%s|sudo:%s#%s/%s"
                   username
                   host
                   port
                   host
                   port
                   path)))))))

(defun ~save-buffer-as (path)
  "Saves current file as."
  (interactive "FPath: ")
  (unless (string-empty-p path)
    (write-file path t)))

(defun ~delete-current-file ()
  "Deletes the file associated with the current buffer and kills
off the buffer."
  (interactive)
  (let ((current-file buffer-file-name))
    (when (and (file-exists-p current-file)
               (yes-or-no-p (concat "Delete file: " current-file)))
      ;; Prevent the following kill-buffer from recursively calling this
      ;; function
      (when (local-variable-p 'local/delete-on-close)
        (kill-local-variable 'local/delete-on-close))
      (kill-buffer (current-buffer))

      (delete-file current-file)
      (message "%s deleted" current-file))))

;; TODO: Is buffer-local-value necessary?

(defun ~maybe-delete-file-when-killing-buffer ()
  "Deletes current file when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-on-close)
             local/delete-on-close)
    (~delete-current-file)))

(defun ~maybe-delete-frame-when-killing-buffer ()
  "Deletes current frame when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-frame-on-close)
             local/delete-frame-on-close)
    (delete-frame local/delete-frame-on-close)))

(defun ~maybe-delete-window-when-killing-buffer ()
  "Deletes current window when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-window-on-close)
             local/delete-window-on-close)
    (delete-window)))

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

(defun ~delete-linked-windows ()
  "Deletes the linked windows, which are store in the local
variable `local/linked-windows'."
  (interactive)
  (when (local-variable-p 'local/linked-windows)
    (dolist (window local/linked-windows)
      (unless (or (equalp window (get-buffer-window))
                  (not (window-live-p window)))
        (delete-window window)))))

(defun ~clean-up-tramp ()
  "Closes all tramp connections and buffers."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(defun ~get-current-project-root ()
  "Returns the current project root or current directory."
  (or (ignore-errors (destructuring-bind (_ . dir) (project-current)
                       dir))
      default-directory))

(cl-defun ~counsel-grep-default-project-root (&optional (counsel-grep-fn #'counsel-rg))
  "Calls a counsel-grep function, taking project root by default
and fallback to current directory if project root is not found."
  (interactive)
  (if current-prefix-arg
      (call-interactively counsel-grep-fn)
    (funcall counsel-grep-fn nil (~get-current-project-root))))

(defun ~current-dir ()
  "Current directory or `$HOME`."
  (or (file-name-directory (or load-file-name buffer-file-name ""))
      "~"))

(defun ~current-file-full-path ()
  "Full path to current file."
  (or (expand-file-name buffer-file-name)
      ""))

(defun ~counsel-rg ()
  "Executes `counsel-rg' with pre-selected input from current
selection."
  (interactive)
  (if (~is-selecting?)
      (counsel-rg (~get-selection))
    (call-interactively #'counsel-rg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~add-to-history-file (history-path line &key (max-history 1000))
  "Saves a line to a history file."
  (~exec-|-async (concat line "\n")
                 ("add-to-history" "--max-history" (number-to-string max-history) history-path)))

(cl-defun ~add-arg-to-history-fn (history-path action-fn &key (max-history 3000))
  "Returns a function that records text to a history file, then performs the action defined by ACTION-FN."
  (lexical-let* ((history-path history-path)
                 (action-fn action-fn)
                 (max-history max-history))
    #'(lambda (text)
        (~add-to-history-file history-path text
                              :max-history max-history)
        (funcall action-fn text))))

(defun ~choose-from-history (history-path)
  "Prompts for an entry from a history file.  A history file is a
text file with each line corresponding to an entry.  An entry is
trimmed after reading."
  (let* ((content (~read-file history-path))
         (lines (split-string content "\n")))
    (string-trim (ivy-read "Command: " lines))))

(defun ~insert-from-history (history-path)
  "Prompts for an entry and inserts it to the current buffer.
  See `~CHOOSE-FROM-HISTORY' for further information on the
  history file and how an entry is formatted."
  (interactive "fHistory path: ")
  (let ((entry (~choose-from-history history-path)))
    (unless (string-empty-p entry)
      (insert entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~is-next-line-output-block? ()
  "Determines if the next line is the start of an output block.
The output block is defined as everything between
*~OUTPUT-BEGINNING-MARKER* and *~OUTPUT-END-MARKER*."
  (save-mark-and-excursion
    (ignore-errors
      (next-line)
      (beginning-of-line)
      (looking-at (rx bol (0+ space)
                      (eval *~output-beginning-marker*)
                      (0+ space) eol)))))

(defun ~current-line-continues? ()
  "Determines if the current line has a continuation marker."
  (thread-last (thing-at-point 'line)
    string-trim
    (s-matches? (rx "\\" (0+ space) eol))))

(defun ~previous-line-continues? ()
  "Determines if the previous line has a continuation marker."
  (unless (zerop (line-number-at-pos))
    (save-mark-and-excursion
      (forward-line -1)
      (~current-line-continues?))))

(cl-defun ~shorten-string (str max-length &optional (ellipsis "..."))
  "Shortens a string, making sure its length does not exceed
MAX-LENGTH by truncating and prefixing it with ELLIPSIS if
necessary."
  (let ((actual-length (length str)))
    (if (> actual-length max-length)
        (s-concat ellipsis (substring str (+ (- actual-length
                                                max-length)
                                             (length ellipsis))))
      str)))

(defun ~copy-to-clipboard (text)
  "Copies text to clipboard."
  (kill-new text)
  (message "Copied '%s' to the clipboard." text))

(defun ~ansi-colorize-region ()
  "ANSI-colorizes a region using overlays."
  (interactive)
  (ansi-color-apply-on-region (region-beginning) (region-end)))

(defun ~ansi-colorize-buffer ()
  "ANSI-colorizes current buffer using overlays."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defalias '~ansi-colorize 'ansi-color-apply
  "ANSI-colorizes a string with text properties.")

(defun ~eval-string (str)
  "Evals a string."
  (interactive "sString: ")
  (eval (first (read-from-string (concat "(progn " str ")")))))

(cl-defun ~eval-region ()
  "Evals region and returns value."
  (interactive)
  (if (~is-selecting?)
      (thread-first (buffer-substring (region-beginning) (region-end))
        ~eval-string)
    (error "No region to eval")))

(defun ~eval-current-sexp ()
  "Evals the current enclosing sexp."
  (interactive)
  (require 'expand-region)
  (let ((current-point (point)))
    (call-interactively #'er/mark-outside-pairs)
    (let ((res (call-interactively #'~eval-region)))
      (prin1 res)
      (goto-char current-point)
      (setq deactivate-mark t)
      res)))

(defun ~eval-last-sexp-pp ()
  "Evals the the last sexp and pprints to a buffer."
  (interactive)
  (let ((content (pp-to-string (eval (pp-last-sexp) lexical-binding)))
        (buffer (get-buffer-create "*emacs-lisp-eval*")))
    (~popup-buffer :buffer buffer
                   :content content
                   :working-dir default-directory)))

(defun ~eval-last-sexp-or-region ()
  "Evals region if active, or evals last sexpr."
  (interactive)
  (if (~is-selecting?)
      (call-interactively #'~eval-region)
    (call-interactively #'eval-last-sexp)))

(defun ~eval-then-replace-region-or-last-sexp ()
  "Evals then replaces region or last sexp with result."
  (interactive)
  (let ((value (~eval-last-sexp-or-region)))
    (if (~is-selecting?)
        (delete-region (region-beginning) (region-end))
      (kill-sexp -1))
    (insert (format "%s" value))))

(defun ~gen-uuid ()
  "Generates a UUID."
  (string-trim (~exec-sh (list "uuidgen" "-r"))))

(cl-defun ~gen-filename (&key (separator "-")
                              (word-counter 4))
  "Generates a random file name."
  (string-trim (~exec-sh (list "gen-filename"
                               (number-to-string word-counter)
                               separator))))

(cl-defun ~get-thing-to-execute-from-context ()
  "Retrieves the thing to execute from the current context.  The
rules are as follows:

- If the secondary selection is active, take it;
  otherwise

- if the primary selection is active, take it;
  otherwise

- if the last command is a mouse event, take the thing at the
  mouse cursor;

- if the current line corresponds to a path, take it;

- otherwise take the current symbol or the last sexp at point."
  (interactive)
  (or (~get-secondary-selection)
      (and (region-active-p) (~get-selection))
      (~try-getting-current-thing)))

(cl-defun ~execute (&optional thing
                              &key
                              (exec-fn #'wand:execute)
                              (selection-fn #'~get-thing-to-execute-from-context))
  "Interactively executes THING which is a piece of text or an
sexp using `exec-fn' and return the result.  If THING is not
provided, calls and takes the return value of SELECTION-FN as
THING."
  (interactive)
  (let ((thing (if (null thing)
                   (funcall selection-fn)
                 thing)))
    (when (null thing)
      (error "Nothing to execute"))

    (let* ((result (if (consp thing)
                       (eval thing)
                     (funcall exec-fn thing)))
           (result-str (if (stringp result)
                           result
                         (format "%s" result))))
      result)))

(cl-defun ~execute-text-prompt ()
  "Prompts for text and executes it with `~execute'."
  (interactive)
  (defvar *~execute-text-prompt-hist* (list))
  (let ((text (read-from-minibuffer "Text: " nil nil nil '*~execute-text-prompt-hist*)))
    (~execute text)))

(cl-defun ~execute-line ()
  "Executes current line with `~execute'."
  (interactive)
  (~execute (string-trim (thing-at-point 'line t))))

(cl-defun ~execute-current-wand-text ()
  "Executes current Wand text with `~execute'."
  (interactive)
  (end-of-thing 'wand-text)
  (~execute (string-trim (thing-at-point 'wand-text t))))

(cl-defun ~execute-pop-up (&optional thing
                                     &key
                                     (exec-fn #'wand:execute)
                                     (selection-fn #'~get-thing-to-execute-from-context))
  "Interactively executes THING which is a piece of text or an
sexp using `exec-fn' and pops up the result when done.  If THING
is not provided, calls and takes the return value of SELECTION-FN
as THING."
  (interactive)
  (lexical-let ((output-buffer (format "* output :: %s :: %s *"
                                       thing
                                       (~gen-filename)))
                (workdir default-directory)
                (result (~execute thing :exec-fn exec-fn
                                  :selection-fn selection-fn)))
    (~popup-buffer :buffer output-buffer
                   :content result
                   :working-dir workdir)))

(with-eval-after-load "evil"
  (cl-defun ~advice/evil-ret-execute (orig-fn &rest args)
    "Prevents `evil-ret' in Evil normal-mode and visual-mode to
move the cursor but rather to call `~execute'."
    (interactive)
    (require 'thingatpt)
    (cond ((evil-visual-state-p)
           (let ((thing (if (eq 'line (evil-visual-type))
                            (string-trim-right (thing-at-point 'line))
                          (~get-selection))))
             (~execute thing)))
          ((evil-normal-state-p)
           (lexical-let* ((current-point (point))
                          (res (apply orig-fn args)))
             (unless (= current-point (point))
               (goto-char current-point)
               (setq res (call-interactively #'~execute)))
             res))
          (t
           (apply orig-fn args)))))

;; TODO: History
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

(defun ~read-command-or-get-from-selection (history-file &optional cmd)
  "Reads a free-from text-based command from the minibuffer.  If
secondary selection or primary selection is active, returns one
of them (in that order) instead of reading from the minibuffer."
  (interactive)
  (lexical-let ((history (thread-last (~read-file history-file)
                           string-trim
                           (s-split "\n"))))
    (string-trim
     (if (not (null cmd))
         cmd
       (or (if (string-empty-p (~get-secondary-selection))
               nil
             (~get-secondary-selection))
           (if (string-empty-p (~get-selection))
               nil
             (~get-selection))
           (read-shell-command "Command: "
                               nil
                               'history))))))

;; TODO: Parameterize the hard-coded "!" character.
(defun ~bounds-of-wand-text-at-point ()
  "Returns boundaries for a wand-text thing.  See
`THING-AT-POINT' for futher information."
  (lexical-let* ((start (ignore-errors
                          (save-mark-and-excursion
                            (end-of-line)
                            (re-search-backward (rx bol "!"))
                            (point))))
                 (end (ignore-errors
                        (save-mark-and-excursion
                          (cond
                           (;; When there's no line continuation
                            (not (~current-line-continues?))
                            (end-of-line)
                            (point))
                           (t
                            (while (~current-line-continues?)
                              (re-search-forward (rx "\\" (0+ space) eol) nil t)
                              (forward-line))
                            (end-of-line)
                            (point)))))))
    (if (or (null start) (null end))
        nil
      (cons start end))))
(put 'wand-text 'bounds-of-thing-at-point '~bounds-of-wand-text-at-point)

(defun ~bounds-of-exec-text-at-point ()
  "Returns boundaries for a possibly multiline piece of text that
could be executed.  See `THING-AT-POINT' for futher information."
  (lexical-let* ((start (ignore-errors
                          (save-mark-and-excursion
                            (cond
                             (;; When there's no line continuation on the
                              ;; previous line
                              (not (~previous-line-continues?))
                              (beginning-of-line)
                              (point))
                             (t
                              (while (~previous-line-continues?)
                                (forward-line -1))
                              (beginning-of-line)
                              (point))))))
                 (end (ignore-errors
                        (save-mark-and-excursion
                          (cond
                           (;; When there's no line continuation
                            (not (~current-line-continues?))
                            (end-of-line)
                            (point))
                           (t
                            (while (~current-line-continues?)
                              (re-search-forward (rx "\\" (0+ space) eol) nil t)
                              (forward-line))
                            (end-of-line)
                            (point)))))))
    (if (or (null start) (null end))
        nil
      (cons start end))))
(put 'exec-text 'bounds-of-thing-at-point '~bounds-of-exec-text-at-point)

(use-package thing-cmds)
(defun ~select-multiline-exec-text ()
  "Selects multiline exec text."
  (interactive)
  (mark-things 'exec-text))

(defun ~toggle-popup-exec-result ()
  "Toggles whether or not result from an exec function is popped
up in a separate frame."
  (interactive)
  (setf *~popup-exec-result?* (not *~popup-exec-result?*)))

(cl-defun ~get-block-positions (beginning-regexp end-regexp)
  "Returns multiple values corresponding to the beginning and end
positions of the current block, defined by the regexps
BEGINNING-REGEXP and END-REGEXP."
  (save-excursion)
  (values (save-mark-and-excursion
            (end-of-line)
            (search-backward-regexp beginning-regexp)
            (point))
          (save-mark-and-excursion
            (beginning-of-line)
            (search-forward-regexp end-regexp)
            (point))))

(cl-defun ~mark-block (beginning-regexp end-regexp &key
                                        (mark-fences? nil))
  "Marks a block.  The block is fenced with the regexps
`BEGINNING-REGEXP' and `END-REGEXP'.

If `MARK-FENCES?' is non-nil, marks the fences as well;
otherwise, marks only the content of the block."
  (interactive)
  ;; Start from the end of the block
  (beginning-of-line)
  (search-forward-regexp end-regexp)

  (if mark-fences?
      (forward-line)
    (beginning-of-line))

  (push-mark (point) t t)
  (search-backward-regexp beginning-regexp)

  (if mark-fences?
      (beginning-of-line)
    (next-line))

  (beginning-of-line))

(defun ~mark-current-block ()
  "Marks the current code block."
  (interactive)
  (multiple-value-bind
      (beginning-regexp end-regexp)
      (cond ((eq 'adoc-mode major-mode)
             (values (rx bol "----" (0+ " ") eol) (rx bol "----" (0+ " ") eol)))
            ((eq 'org-mode major-mode)
             (values (rx "#+BEGIN_SRC") (rx "#+END_SRC")))
            (t
             (values (rx "### ««« ###") (rx  "### »»» ###"))))
    (~mark-block beginning-regexp end-regexp)))

(defun ~mark-current-output-block ()
  "Marks the current output block, including the fences."
  (interactive)
  (let ((beginning-regexp (if (boundp 'local/output-beginning-regexp)
                              local/output-beginning-regexp
                            (rx "### ««« ###")))
        (end-regexp (if (boundp 'local/output-end-regexp)
                              local/output-end-regexp
                            (rx "### »»» ###"))))
    (~mark-block beginning-regexp end-regexp
                 :mark-fences? t)))

(defun ~goto-next-line-matching-marker ()
  "Goes to the next line matching a visual marker (defined by
`*~MARKER-REGEXP*')"
  (interactive)
  (search-forward-regexp *~marker-regexp* nil t))

(defun ~goto-prev-line-matching-marker ()
  "Goes to the previous line matching a visual marker (defined by
`*~MARKER-REGEXP*')"
  (interactive)
  (search-backward-regexp *~marker-regexp* nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes/Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~firefox (url &key (new-window? nil))
  "Opens a URL in Firefox."
  (interactive
   (list (read-string "URL: " (cond
                               ((~is-selecting?)
                                (~get-selection))
                               ((thing-at-point-url-at-point)
                                (thing-at-point-url-at-point))
                               (t
                                "https://encrypted.google.com/")))))
  (let ((url (shell-quote-argument url)))
   (~run-process (if new-window?
                     (format "firefox --new-window %s" url)
                   (format "firefox %s" url)))))

(defun ~get-process-output (process)
  "Gets the output for a managed process."
  (with-current-buffer (process-buffer process)
    (buffer-string)))

(cl-defun ~run-process (command &key (async t))
  "Runs an external process.  If `async' is non-`nil' the process
is not terminated when Emacs exits and the output is discarded;
otherwise, both output from stdout and stderr are direceted to
the buffer naming after `command'.  `command' is executed via the
current user shell, define by the `SHELL' environment variable.
Note that `command' is not automatically quoted and should be
quoted with `shell-quote-argument'."
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
                             ;; Make sure the command doesn't fail, otherwise
                             ;; the finish function never gets called
                             (s-concat command "; true"))
      (call-process current-shell

                    ;; Taking no input
                    nil

                    ;; Output buffer
                    process-name

                    ;; Don't display output buffer
                    nil

                    ;; Arguments
                    "-c" command))))

(cl-defun ~exec-sh (command &key stdin)
  "Executes a *shell* command then returns its output as string.

COMMAND is either a list or a string, denoting the shell command.
In case COMMAND is a string, you might need to quote the shell
arguments inside, e.g. by using `shell-quote-argument'.  If
COMMAND is a list, it is then quoted with `shell-quote-argument'
automatically and joined to a string.

STDIN determines where to read standard input for the shell
command.  Its value type is one of the following:

* nil → no stdin;

* :region → stdin is taken from the current region;

* any other value → stdin is that value."
  (interactive "MCommand: ")
  (lexical-let ((command (typecase command
                           (string command)
                           (list (string-join (loop for arg in command
                                                    collect (shell-quote-argument arg))
                                              " "))
                           (t (error "COMMAND must be a string or a list")))))
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
               (save-mark-and-excursion
                 (with-current-buffer command
                   (ansi-color-apply-on-region (point-min)
                                               (point-max))))
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
                   (save-mark-and-excursion
                     (with-current-buffer command
                       (ansi-color-apply-on-region (point-min)
                                                   (point-max))))
                   (~get-buffer-content command)))))

(cl-defun ~exec-pop-up (command)
  "Executes a command asynchronously & pops up a temporary buffer
showing result when done.  COMMAND is a list of strings, denoting
the command.

This function returns corresponding asynch process."
  (interactive)
  (lexical-let ((command-str (thread-first (loop for arg in command
                                                 collect (shell-quote-argument arg))
                               (string-join " ")
                               string-trim)))
    (~add-to-history-file *~exec-history-path* command-str
                          :max-history *~exec-history-max*)
    (~exec-async command
                 :output-callback #'(lambda (output-buffer)
                                      (with-current-buffer output-buffer
                                        (~ansi-colorize-buffer))
                                      (pop-to-buffer output-buffer))
                 :output-as-buffer t
                 :keep-output-buffer t)))

(cl-defun ~exec-sh-pop-up (command)
  "Similar to `~exec-pop-up' but COMMAND is a string denoting a
shell command.  This allows the use of shell operators in
COMMAND.  Note that shell arguments might need quoting, e.g. with
`shell-quote-argument'."
  (interactive "MCommand: ")
  (lexical-let ((command (string-trim command)))
    (~add-to-history-file *~exec-history-path* command
                          :max-history *~exec-history-max*)
    (~exec-pop-up (list shell-file-name "-c" command))))

(cl-defun ~exec-pop-up.old (command)
  "Executes a command & pops up a temporary buffer showing
result, returing the process.  The command is executed asynchronously."
  (interactive "MCommand: ")
  (let* ((name command)
         (process (start-process-shell-command name name command))
         (dir default-directory))
    (~popup-buffer :buffer name)
    (with-current-buffer name
      (setq-local default-directory dir)
      ;; For some weird reason, we need to sleep shortly before we're able to
      ;; jump to the beginning of the buffer
      (sleep-for 0 1)
      (goto-char (point-min)))
    process))

(cl-defun ~exec-sh< (command &key
                             (move-cursor? t)
                             (print-output-marker? nil)
                             (current-position (point))
                             (destination (point))
                             (callback #'identity))
  "Executes a *shell* command and replaces the region with the output.
This function also returns the exit code of the command.

PRINT-OUTPUT-MARKER? is a boolean value, determining whether or
not to print the output markers (defined by
*~OUTPUT-BEGINNING-MARKER* and *~OUTPUT-END-MARKER*)

CURRENT-POSITION is a number, determining the position at which
the output is printed.

DESTINATION is a number or nil, determining the position at which
the cursor is moved to after the output is printed."
  (interactive "MCommand: ")
  (~add-to-history-file *~exec-history-path* command
                        :max-history *~exec-history-max*)

  (message "Running: %s" command)
  (lexical-let ((current-position current-position)
                (buffer (current-buffer))
                (print-output-marker? print-output-marker?)
                (destination destination)
                (move-cursor? move-cursor?))
    (~exec-pipe-async (:sh shell-file-name "-c" (format "env 'TERM=dumb' 'PAGER=cat' %s" command))
                      (:fn #'(lambda (output)
                               (message "Finished: %s" command)

                               (with-current-buffer buffer
                                 (goto-char current-position)

                                 (~insert-output-block output
                                                       :print-output-marker? print-output-marker?
                                                       :colorize-with :overlay)

                                 (when (and move-cursor? (numberp destination))
                                   (goto-char destination)))))
                      (:fn callback))))

(cl-defun ~prepare-for-output-block (&optional (replace-output? t))
  "TODO"
  (interactive)
  ;; Make sure we're an the end of the command
  (when (region-active-p)
    (goto-char (region-end))
    (end-of-line)
    (deactivate-mark))

  ;; Make sure we're not at the end of the output marker
  (when (looking-back (rx bol (0+ space)
                          (eval *~output-beginning-marker*)
                          (0+ space) eol)
                      nil t)
    (beginning-of-line)
    (backward-char))

  ;; Create a line to separate the command and the output
  (~open-line 1)
  (beginning-of-line)

  ;; Delete the current output block if necessary
  (when (and replace-output? (~is-next-line-output-block?))
    (call-interactively #'~delete-output-block)))

(cl-defun ~exec-sh<-next-line-separate (command &key (replace-output? t)
                                                (original-point (point))
                                                (move-cursor? t)
                                                (callback #'identity))
  "Executes a *shell* command in a newly spawned shell and pipes
back the output to the next line.  The current cursor doesn't
change."
  (interactive)
  (~prepare-for-output-block replace-output?)
  (~exec-sh< command
             :print-output-marker? t
             :current-position (point)
             :destination original-point
             :move-cursor? move-cursor?
             :callback callback))

(defun ~exec-sh> (command)
  "Executes a *shell* command, taking input from the current region,
pops up a temporary buffer showing result, and returns the exit
code of the command."
  (interactive "MCommand: ")
  (~add-to-history-file *~exec-history-path* command
                        :max-history *~exec-history-max*)
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
    (save-mark-and-excursion
      (with-current-buffer command
        (ansi-color-apply-on-region (point-min)
                                    (point-max))))
    (~popup-buffer :buffer command)))

(defun ~exec-sh| (command)
  "Executes a *shell* command, taking input from the current region,
and replaces the region with the output.  This function also
returns the exit code of the command."
  (interactive "MCommand: ")
  (let ((inhibit-message t))
    (shell-command-on-region (region-beginning)
                             (region-end)
                             command

                             ;; Output buffer name
                             command

                             ;; Replace current region
                             t

                             ;; Error piped to output
                             nil

                             nil

                             t)))

(cl-defun ~exec-async (command &key (stdin nil)
                             (coding-system 'undecided)
                             (connection-type 'pipe)
                             output-callback
                             output-as-buffer
                             keep-output-buffer)
  "Executes a program asynchronously and returns the
corresponding async process.

COMMAND is a list of strings representing the program and its
arguments.  Note that the program is NOT executed in a shell (for
that you would need to specificy the shell command manually).
Thus shell operators, e.g. piping, don't work.  For piping,
please have a look at the `~exec-pipe-async', which handles
piping for external commands and Emacs Lisp function.

STDIN determines where the process can take the standard
input (stdin).  It takes one of the following value types:

* nil → empty stdin;

* :region → stdin is the current region;

* :2nd-region → stdin is the current secondary selection;

* a buffer → stdin is the content of that buffer;

* a string → stdin is that string value

Any other value types of STDIN will result in an error by
`make-process'.

CODING-SYSTEM and CONNECTION-TYPE share their meaning with those
from `MAKE-PROCESS' so please check out its documentation for
futher information.

OUTPUT-CALLBACK is a function taking a single value - the output
of the program.  OUTPUT-CALLBACK is called when the program
exits.

OUTPUT-AS-BUFFER is a boolean value that determines whether the
output of the program is passed to OUTPUT-CALLBACK as a string or
as a buffer.  Note that the buffer connecting to the program's
standard output (stdout) and standard error (stderr) is always
created, no matter what value of OUTPUT-AS-BUFFER is.  Hence,
generally speaking, it's preferred to pass t to OUTPUT-AS-BUFFER
and make OUTPUT-CALLBACK takes that buffer for better
performance.

KEEP-OUTPUT-BUFFER is a boolean value, determining whether or not
the output buffer of the program is killed after the program
exits.

E.g.

;; Execute 'ls -l', insert stdout and stderr to the current buffer
\(~exec-async \(list \"ls\" \"-l\"\) :output-callback #'insert\)

;; Execute 'ls -l', insert stdout and stderr to the current
;; buffer a bit more efficiently
\(~exec-async \(list \"ls\" \"-l\"\)
             :output-callback #'\(lambda \(buf\)
                                  \(insert \(with-current-buffer buf \(buffer-string\)\)\)\)
             :output-as-buffer t\)

;; Pipe 'hello world' to 'cat -', then insert it to the current
;; buffer
\(~exec-async \(list \"cat\" \"-\"\)
             :stdin \"hello world\\n\"
             :output-callback #'insert\)
"
  (interactive "MCommand: ")
  (lexical-let* ((name (string-join command " "))
                 (buffer-name (format "*process :: %s :: %s*" name (~gen-filename)))
                 (proc (make-process
                        :name name
                        :buffer buffer-name
                        :command command
                        :coding coding-system
                        :connection-type connection-type
                        :sentinel #'(lambda (proc _event)
                                      (when output-callback
                                        (let ((output (if output-as-buffer
                                                          (process-buffer proc)
                                                        (with-current-buffer (process-buffer proc)
                                                          (buffer-string)))))
                                          (funcall output-callback output)))
                                      (unless (or keep-output-buffer
                                                  (not (buffer-live-p (process-buffer proc))))
                                        (kill-buffer (process-buffer proc)))))))
    (pcase stdin
      ('nil t)
      (:region (process-send-region proc (region-beginning) (region-end))
               (process-send-eof proc))
      (:2nd-region (process-send-string proc (~get-secondary-selection))
                   (process-send-eof proc))
      (stdin-value (if (bufferp stdin-value)
                       (with-current-buffer stdin-value
                         (process-send-region proc (point-min) (point-max)))
                     (process-send-string proc stdin))
                   (process-send-eof proc)))
    proc))

(cl-defmacro ~exec-pipe-async (&rest commands)
  "Executes commands in sequence, piping output (and possible
standard error) from one command to the next one.

Each command is a list of the following format: `\(COMMAND-TYPE
ARGS...\)' or SEXP, where:

* If a command is an sexp (SEXP), it is evaluated in turn.

* If a command is of the other format, COMMAND-TYPE is one of the
  following:

  * :STR → the command is a string.  ARGS should be a single
    string parameter, which is passed as-is as input to the next
    command (if any);

  * :FN → the command is a function call.  The first parameter
    from ARGS is the function and the rest being the arguments
    passed to the function when called.

  * :SH → the command is an external programs.  The first
    parameter from ARGS is the program and the rest being the
    arguments passed to the program.

  * :EXP → the command is an Emacs Lisp expression.  ARGS should
    be a single sexp.

This function is powerful as it allows mixing Emacs Lisp
functions and external programs to process data.  Some examples:

;; Just return \"hello world\"
\(~exec-pipe-async \(:str \"hello world\"\)\)

;; Pipe \"hello world\n\" to 'cat', and insert it back to the
;; current buffer
\(~exec-pipe-async \(:str \"hello world\\n\"\)
                  \(:sh \"cat\" \"-\"\)
                  \(:fn #'insert\)\)

;; Build a context menu using sawfish-menu
\(~exec-pipe-async \(:exp \(with-output-to-string
                          \(print `\(popup-menu \(\(\"_Top level\" 0\)
                                               \(\"_Sub menu\"
                                                \(\"_Foo\" 1\)
                                                \(\"_Bar\" 2\)
                                                \(\"_Quux\" 3\)\)\)\)\)\)\)
                  \(:sh \"/usr/lib/x86_64-linux-gnu/sawfish/sawfish-menu\"\)
                  \(:fn #'insert\)\)

For a bit more simplified, have a look at `~exec-|-async'.

Note on implementation details: this function uses `~exec-async'
to call external programs and uses process buffers for piping for
performance reasons."
  `(funcall ,(loop for command in (reverse commands)
                   for output-callback = (quote #'identity) then next-output-callback
                   for next-output-callback = `(lambda (output-buffer)
                                                 ,(cond

                                                   ;; If is string, command is passed as-is
                                                   ((eq :str (first command))
                                                    `(funcall ,output-callback ,(second command)))

                                                   ;; If is function, command is called then passed
                                                   ((eq :fn (first command))
                                                    `(funcall ,output-callback (funcall ,(second command)
                                                                                        (if (bufferp output-buffer)
                                                                                            (with-current-buffer output-buffer
                                                                                              (buffer-string))
                                                                                          output-buffer))))

                                                   ;; If is an external program, execute it
                                                   ((eq :sh (first command))
                                                    `(~exec-async (list ,@(rest command))
                                                                  :stdin output-buffer
                                                                  :output-callback ,output-callback
                                                                  :output-as-buffer t))

                                                   ;; If is an exp, command is eval'ed then passed
                                                   ((eq :exp (first command))
                                                    `(funcall ,output-callback ,(second command)))
                                                   (t
                                                    `(funcall ,output-callback ,command))))
                   finally (return next-output-callback))
            nil))

(cl-defmacro ~exec-|-async (&rest commands)
  "Wrapper for `~exec-pipe-async', does exactly what
`~exec-pipe-async' does but with simplified syntax - each command
from COMMANDS has its type inferred from the value type:

* If a command is a single string, pass it on;

* If a command is a list with the first element being a string,
  pass it on as an external program;

* If a command is a list starting with FUNCTION or LAMBDA, pass
  it on as a function call;

* Otherwise, pass the command as an sexp.

E.g.

;; Just return \"hello world\"
\(~exec-|-async \"hello world\"\)

;; Pipe \"hello world\\n\" to 'cat', and insert it back to the
;; current buffer
\(~exec-|-async \"hello world\\n\"
               \(\"cat\" \"-\"\)
               #'insert\)

;; Build a context menu using sawfish-menu
\(~exec-|-async \(with-output-to-string
                 \(print `\(popup-menu \(\(\"_Top level\" 0\)
                                      \(\"_Sub menu\"
                                       \(\"_Foo\" 1\)
                                       \(\"_Bar\" 2\)
                                       \(\"_Quux\" 3\)\)\)\)\)\)
               \(\"/usr/lib/x86_64-linux-gnu/sawfish/sawfish-menu\"\)
               #'insert\)\)"
  `(~exec-pipe-async ,@(loop for command in commands
                             collect (cond
                                      ((stringp command)
                                       `(:str ,command))
                                      ((listp command)
                                       (cond
                                        ((stringp (first command))
                                         `(:sh ,@command))
                                        ((or (eq 'function (first command))
                                             (eq 'lambda (first command)))
                                         `(:fn ,command))
                                        (t
                                         command)))
                                      (t
                                       command)))))

(defun ~exec-current-block ()
  "Execs the current code block."
  (interactive)
  (save-mark-and-excursion
    (call-interactively #'~mark-current-block)
    (~exec-sh> "exec-stdin")))

(defun ~insert-entry-from-exec-history ()
  "Inserts an entry from the execution history to the current buffer."
  (interactive)
  (~insert-from-history *~exec-history-path*))

(defun ~dispatch-action (text)
  "Dispatches action based on text.  Ignore output."
  (interactive)
  (message "Dispatching action: %s" text)
  (~exec-|-async ("setsid" "--fork" "dispatch-action" text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core functions")

(provide 'rmacs:config-core-functions)
