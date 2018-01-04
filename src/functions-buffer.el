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

(defun* ~previous-line+ (&optional (n-lines 5))
  "Scrolls up `n-lines'"
  (interactive)
  (previous-line n-lines))

(defun* ~next-line+ (&optional (n-lines 5))
  "Scrolls down `n-lines'"
  (interactive)
  (next-line n-lines))

(with-eval-after-load "helm"
  (defun* ~helm-previous-line+ (&optional (n-lines 5))
    "Scrolls up `n-lines' in Helm mode"
    (interactive)
    (helm-previous-line n-lines))

  (defun* ~helm-next-line+ (&optional (n-lines 5))
    "Scrolls down `n-lines' in Helm mode"
    (interactive)
    (helm-next-line n-lines)))

;;
;; https://www.emacswiki.org/emacs/UnfillParagraph
;;

(defun ~unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun ~current-char ()
  "Return the string representing the character at the current
cursor position."
  (~get-text (point) (+ 1 (point))))

(defun ~peek-char ()
  "Peek next character, return the string representing it.."
  (~get-text (+ 1 (point)) (+ 2 (point))))

(defun ~join-with-next-line ()
  "Join next line with the current line.  This is just a
convenient wrapper of `join-line'."
  (interactive)
  (join-line -1))

(defun ~selection-start ()
  "Return the position of the start of the current selection."
  (region-beginning))

(defun ~selection-end ()
  "Return the position of the end of the current selection."
  (region-end))

(defun ~is-selecting? ()
  "Determine if a selection is being held."
  (use-region-p))

(defun ~current-selection ()
  "Return the current selected text."
  (if (region-active-p)
    (buffer-substring (~selection-start)
                      (~selection-end))
    ""))

(defun ~parenthesize-last-sexp ()
  "Parenthesize last sexp in and using Paredit mode."
  (interactive)
  (paredit-backward)
  (paredit-wrap-round))

(defun ~get-secondary-selection ()
  "Gets the secondary selection (by default, activated with M-Mouse-1)."
  (x-get-selection 'SECONDARY))

(defun ~read-command-or-get-from-secondary-selection ()
  "Without prefix argument, if there is an active selection,
returns it (assuming that it denotes a shell command);
otherwise, reads and returns a shell command from the
minibuffer.

With prefix argument, always reads the shell command from the
minibuffer."
  (interactive)
  (if (and (~get-secondary-selection) (not current-prefix-arg))
      (~get-secondary-selection)
    (read-shell-command "Command: ")))

(defun ~surround (begin-string end-string)
  "Surround current selection with `begin-string` at the
beginning and `end-string` at the end.  If selection is not
active, insert `begin-string` and `end-string` and place the
cursor in-between them."
  (interactive "sStart string: \nsEnd string: ")
  (cond
   ((region-active-p)
    (save-excursion
      (let ((start-point (region-beginning))
            (end-point   (region-end)))
        (goto-point start-point)
        (insert begin-string)
        (goto-point end-point)
        (forward-char (length begin-string))
        (insert end-string))))

   (t
    (insert (concat begin-string end-string))
    (backward-char (length end-string)))))

(defun ~find-file-extended (&optional dir-path)
  "Find file with `fiplr' in a directory.  Symlinks are
followed."
  (interactive)
  (if (require 'fiplr nil nil)
      (let ((path (cond ((not (null dir-path))
                         dir-path)
                        ((~is-selecting?)
                         (~get-selection))
                        (t
                         (read-directory-name "Directory path: ")))))
        (fiplr-find-file-in-directory (file-chase-links path) fiplr-ignored-globs))
    (message "You need `fiplr' package to use this function.")))

(defun ~counsel-ag-default-project-root ()
  "Calls `counsel-ag', taking project root by default and
fallback to current directory if project root is not found."
  (interactive)
  (if current-prefix-arg
      (call-interactively 'counsel-ag)
    (counsel-ag nil (or (ignore-errors (projectile-project-root))
                        default-directory))))

(defun ~find-file-new-frame (path &optional wildcards)
  "Calls `find-file' in a new frame."
  (let ((frame (make-frame)))
    (select-frame frame)
    (find-file path wildcards)))

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

(defun* toolbox:open-file-specialized (file-pattern &key (new-frame? nil))
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

(defun* toolbox:open-file (path &key (new-frame? nil))
  "Opens path and open with external program if necessary."
  (dolist (regexp&action (append (if (boundp '*open-with-regexps*)
                                     *open-with-regexps*
                                   (list))
                                 (list '(".*" . (lambda (path)
                                                  (toolbox:open-file-specialized path
                                                                                 :new-frame? new-frame?))))))
    (let ((regexp (car regexp&action))
          (action (cdr regexp&action)))
      (when (s-matches-p regexp path)
        (return (typecase action
                  (function   (funcall action path))
                  (string     (toolbox:open-with path action))
                  (otherwise  (message-box (format "Invalid program %s" action)))))))))

(defun toolbox:open-with (file cmd)
  "Opens file with a command line.  File name is quoted
automatically quoted."
  (toolbox:run-process (format cmd file)))

(defun toolbox:execute-and-replace ()
  "Execute command on selection using `wand:execute' then replace
selection with command output."
  (interactive)
  (let* ((text (save-excursion
                (get-selection)))
         (output (wand:execute text)))
    (call-interactively 'kill-region)
    (insert output)))

(defun ~toggle-split-method ()
  "Toggle default split method between vertical split and
horizontal split."
  (interactive)
  (cond (split-width-threshold
         (setq *split-width-threshold-old* split-width-threshold)
         (setq split-width-threshold nil))
        (t
         (setq split-width-threshold *split-width-threshold-old*))))

(defun ~adoc-make-bold ()
  "Boldifies selection in adoc-mode."
  (interactive)
  (when (region-active-p)
    (~surround "**" "**")))

(defun ~move-to-compilation-buffer ()
  "Move to *compilation* buffer if it exists."
  (interactive)
  (if (find "*compilation*" (mapcar #'buffer-name (buffer-list))
            :test #'equal)
      (switch-to-buffer "*compilation*")))

(defun ~current-buffer-name ()
  "Retrieve the name of the current buffer."
  (buffer-name (current-buffer)))

(defun ~get-buffer-major-mode (buffer-or-name)
  "Retrieve the buffer's major mode."
  (with-current-buffer buffer-or-name
    major-mode))

(defun ~kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; (defun ~switch-to-last-buffer ()
;;   "Switch to last buffer."
;;   (interactive)
;;   (let ((old-name (~current-buffer-name)))
;;     (switch-to-buffer (other-buffer))
;;     (while (or (s-starts-with? "*" (~current-buffer-name))
;;                (s-equals? old-name (~current-buffer-name)))
;;       (previous-buffer))))
(defalias '~switch-to-last-buffer 'mode-line-other-buffer
  "Switches to the most recently visited buffer.")

(defun ~last-sexp ()
  "Return the sexp right before the current cursor."
  (interactive)
  (preceding-sexp))

(defun ~current-line-number ()
  "Return current line number in the buffer."
  (interactive)
  (count-lines 1 (point)))

(defun ~current-line-comment-syntax ()
  "Return the current line-comment syntax for current buffer mode."
  comment-start)

(defun* ~popup-message (content &key (buffer-name "*Temporary*"))
  "Display a popup window with CONTENT as its content and an
optional BUFFER-NAME name.  Require popwin extension.  Press ESC
or C-g to close the window.

E.g.

;; Display \"Hello World\" in a popup window.
\($popup-message \"Hello World\"\)

;; Display \"Hola Mundo\" in a popup window, naming that window buffer \"*mundo*\"
\($popup-message \"Hello World\" :buffer-name \"*mundo*\"\)
"
  (with-output-to-temp-buffer buffer-name
    (princ content)))

(defun ~goto-snippets-dir ()
  "Go to personal snippets directory."
  (interactive)
  (find-file *snippet-dir*))

(defun ~switch-to-scratch ()
  "Switches to `scratch.el' in `*scratch-dir*' directory in
another window."
  (interactive)
  (let ((scratch-dir (or *scratch-dir* temporary-file-directory)))
    (unless (string-equal "scratch.el" (file-name-nondirectory buffer-file-name))
      (if (get-buffer "scratch.el")
          (switch-to-buffer-other-window "scratch.el")
        (find-file-other-window (s-concat scratch-dir "scratch.el"))))))

(defun* ~switch-to-messages-buffer (&key (in-other-window t))
  "Switches to the `*Messages*' buffer."
  (interactive)
  (if in-other-window
      (switch-to-buffer-other-window "*Messages*")
    (switch-to-buffer "*Messages*")))

;;; TODO
(defun ~undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the
nth-killed buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
         (car recently-killed-list)))))

(defun ~new-buffer ()
  "Open a new empty buffer.  Thanks Xah Lee for this function.
Reference:
http://ergoemacs.org/emacs/emacs_new_empty_buffer.html"
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq default-directory (if (boundp '*scratch-dir*)
                                *scratch-dir*
                              "/m/scratch/"))
    (set-visited-file-name (format "%s_%s"
                                   (s-trim (~exec "now-standardized"))
                                   (s-trim (~exec "uuidgen"))))
    (setq-local local/delete-on-exit t)
    (add-file-local-variable 'local/delete-on-exit t)
    (beginning-of-buffer)
    (setq buffer-offer-save t)))

(defun ~insert-full-line-comment ()
  "Inserts a line full of comment characters until `fill-column' is reached."
  (interactive)
  (let ((comment (s-trim comment-start)))
    (->> (loop for time from (current-column) upto (1- fill-column) by (length comment)
               collect comment)
         (s-join "")
         insert)))

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

(defun ~open-file-if-existed (path)
  "Opens file if exists of displays a message notifying that the
file does not exist."
  (typecase path
    (string (if (file-exists-p path)
                (find-file path)
              (message "File %s doesn't exist" path)))
    (otherwise (message "Invalid file path: %s" path))))

(defun ~is-selecting? ()
  "Determine if a selection is being held."
  (region-active-p))

(defun ~toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case, in that
cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (unless (eq last-command this-command)
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]")
          (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]")
          (put this-command 'state "init caps"))
         (t (put this-command 'state "all lower"))
         )))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2)
      (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2)
      (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2)
      (put this-command 'state "all lower")))))

(defun ~jsify-var (str)
  "JavaScript-ifies a module name by CaMElizing it and stripping
extension and basename.

E.g.

\(~jsify-var \"hello\"\)                  ⇨ hello
\(~jsify-var \"hello-world\"\)            ⇨ helloWorld
\(~jsify-var \"../hello-world\"\)         ⇨ helloWorld
\(~jsify-var \"../../hello-world\"\)      ⇨ helloWorld
\(~jsify-var \"../aoeu/hello-world\"\)    ⇨ helloWorld
\(~jsify-var \"../aoeu/hello-world.js\"\) ⇨ helloWorld.js"
  (first (-reduce-from (lambda (hold item)
                         (let* ((res (first hold))
                                (needs-cap (second hold))
                                (next-item (if needs-cap
                                               (capitalize item)
                                             item)))
                           (if (string-equal "-" item)
                               (list res t)
                             (list (format "%s%s" res next-item)
                                   nil))))
                       (list "" nil)
                       (-> (split-string str "/")
                           (-last-item)
                           (split-string "")))))

(defun ~select-sexp ()
  "Binds double click mouse-1 to select current sexp."
  (interactive)
  (cond ((eq ?\( (char-after))
         (mark-sexp))
        ((or (~line-match? (format "^ *%s+" comment-start))
             (~line-match? "^ +$"))
         (mark-line))
        (t
         (paredit-backward-up)
         (mark-sexp))))

(defun ~list-buffers ()
  (interactive)
  (helm-mini))

(defun ~open-local-toolbox ()
  "Open the toolbox.el in current directory."
  (interactive)
  (find-file "toolbox.el"))

(defun ~clone-file ()
  "Clone current file into a buffer."
  (interactive)
  (copy-to-register 'clone
                    (save-excursion
                      (beginning-of-buffer)
                      (point))
                    (save-excursion
                      (end-of-buffer)
                      (point)))
  (call-interactively 'multi-scratch-new)
  (insert-register 'clone)
  (keyboard-quit))

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

(defun ~fix-hard-wrapped-region (begin end)
  "Fix hard-wrapped paragraphs."
  (interactive "r")
  (shell-command-on-region begin end "fmt -w 2500" nil t))

(defun ~get-selection ()
  "Return the current selected text."
  (~current-selection))

(defun ~delete-selected-text ()
  "Delete the selected text, do nothing if none text is selected."
  (if (~is-selecting?)
    (delete-region (~selection-start) (~selection-end))))

(defun ~replace-selection (&optional text)
  "Replace selection with text."
  (interactive)
  (let ((text (if (stringp text)
                text
                (read-string "Text: "))))
    (call-interactively 'delete-region)
    (insert-string text)))

(defun ~goto-selection-start ()
  "Go to the start of current selection.  If selection is not active,
do nothing."
  (if (~is-selecting?)
    (goto-point (~selection-start))))

(defun ~move-to-beginning-of-line ()
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line."
  (interactive)
  (let ((orig-point (point)))
    (unless visual-line-mode
      (back-to-indentation))
    (when (= orig-point (point))
      (beginning-of-visual-line nil))))

(defun ~delete-line ()
  "Delete current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(defun ~get-text (start end)
  "Return text from current buffer between start and end point."
  (if (or (< start (point-min))
          (< (point-max) end))
    ""
    (buffer-substring start end)))

(defun ~goto-selection-end ()
  "Go to the end of current selection.  If selection is not active,
do nothing."
  (if (~is-selecting?)
    (goto-point (~selection-end))))

(defun ~mark-line ()
  "Marks current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line))

(defun ~electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match'
then open and indent an empty line between the cursor and the
text.  Move the cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at *electrify-return-match*)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun ~kill-line-delete-spaces ()
  "Kill current line and delete horizontal spaces."
  (interactive)
  (kill-line)
  (delete-horizontal-space))

(defun* ~scroll-other-window (&key (nlines 5))
  (interactive)
  (scroll-other-window nlines))

(defun* ~scroll-other-window-reverse (&key (nlines 5))
  (interactive)
  (scroll-other-window (- nlines)))

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

(defun revert-all-buffers ()
  "Refreshes all openen file buffers if they are not modified."
  (interactive)
  (->> (buffer-list)
       (-filter '(lambda (buffer)
                   (let ((current-file-name (buffer-file-name buffer)))
                     (and current-file-name
                          (file-exists-p current-file-name)
                          (not (buffer-modified-p buffer))))))
       (-map '(lambda (buffer)
                (with-current-buffer buffer
                  (revert-buffer t t t)))))
  (message "Refreshed opened file buffers"))

(defun ~make-executable ()
  "chmod +x current file."
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
   (message
    (concat "Saved as script: " buffer-file-name))))

(defun ~clean-up-tramp ()
  "Closes all tramp connections and buffers."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(defun ~insert-text-at-the-end (&optional text)
  "Insert current selected text at the end of current buffer."
  (interactive)
  (let ((text (cond ((stringp text)
                     text)
                    ((~is-selecting?)
                     (~current-selection))
                    (t
                     (read-string "Text: ")))))
    (call-interactively 'end-of-buffer)
    (insert-string text)))

(defun* ~inc-string (str &optional (amount 1))
  "Converts a string into number, increases it and converts back
to string.  If `STR' is not a valid number, returns the amount to
add."
  (number-to-string (+ amount (typecase str
                                (string (string-to-number str))
                                (number str)
                                (otherwise 0)))))

(defun ~load-paredit-mode ()
  "Loads paredit mode and disable other pairing modes."
  (paredit-mode t)
  (when (fboundp 'autopair-mode)
    (autopair-mode 0))
  (when (fboundp 'smartparens-mode)
    (smartparens-mode 0)))

(defun ~activate-evil-local-mode ()
  "Activate evil local mode."
  (interactive)
  (toggle-evil-local)
  (setq-default cursor-type 'hbar)
  (setq-default cursor-type 'bar))

(defun ~markdown-italicize ()
  "Italicize selection or adding italic format."
  (interactive)
  (~surround "*" "*"))

(defun ~markdown-embolden ()
  "Embolden selection or adding bold format."
  (interactive)
  (~surround "**" "**"))

(defun ~markdown-rawify ()
  "Rawify selection or adding raw format."
  (interactive)
  (~surround "`" "`"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~one-window ()
  "Delete all other non-dedicated windows."
  (interactive)
  (mapcar #'(lambda (window)
              (unless (window-dedicated-p window)
                (delete-window window)))
          (cdr (window-list))))

(defun ~delete-window ()
  "Delete current window if it's not sticky/dedicated.  Use
prefix arg (`C-u') to force deletion if it is."
  (interactive)
  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Window '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))
      (delete-window (selected-window))))

(defun ~toggle-sticky-window ()
  "Toggle stickiness of the currently active window."
  (interactive)

  (let* ((window  (get-buffer-window (current-buffer)))
         (sticky? (window-dedicated-p window)))
    (set-window-dedicated-p window (not sticky?))
    (message (if (not sticky?)
                 "Window '%s' is now sticky"
               "Window '%s' is now normal")
             (current-buffer))))

(defun ~toggle-maximize-buffer ()
  "Toggles maximization of current buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;; FIXME

(defun* ~layout/default ()
  "Saves the current window configuration and keep only a single window"
  (interactive)
  (~one-window))

(defun* ~layout/hsplit ()
  "Activate my personal horizontal split layout."
  (interactive)
  (~layout/default)
  (split-window-horizontally)
  (call-interactively 'other-window)
  (next-buffer)
  (while (null (buffer-file-name (current-buffer)))
    (next-buffer))
  (call-interactively 'other-window))

(defun* ~layout/vsplit ()
  "Activate my personal vertically split layout."
  (interactive)
  (~layout/default)
  (split-window-vertically)
  (call-interactively 'other-window)
  (next-buffer)
  (while (null (buffer-file-name (current-buffer)))
    (next-buffer))
  (call-interactively 'other-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '~smart-forward-exp 'forward-word
  "Forward word")

(defalias 'current-point 'point
  "Return current position of the keyboard cursor in the
buffer.")

(defalias '~current-word 'current-word
  "Return the current word as string.")

(defalias 'goto-point 'goto-char
  "Goto `point` in the buffer")

(defalias 'incs '~inc-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xah Lee's open last buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xah-recently-closed-buffers
  nil
  "alist of recently closed buffers. Each element is (buffer
name, file path). The max number to track is controlled by the
variable `xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max
  40
  "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer if it's not sticky/dedicated.  Use
prefix arg (`C-u') to force closing if it is.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the
  buffer is not associated with a file.

• Make sure the buffer shown after closing is a user buffer.

• If the buffer is editing a source file in an org-mode file,
  prompt the user to save before closing.

• If the buffer is a file, add the path to the list
  `xah-recently-closed-buffers'.

• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.  Else it is a
user buffer."
  (interactive)

  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Buffer '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))

      (let (ξemacs-buff-p
            (ξorg-p (string-match "^*Org Src" (buffer-name))))

        (setq ξemacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

        (if (string= major-mode "minibuffer-inactive-mode")
            (minibuffer-keyboard-quit)  ; if the buffer is minibuffer
          (progn
            ;; offer to save buffers that are non-empty and modified, even for
            ;; non-file visiting buffer. (because kill-buffer does not offer to
            ;; save buffers that are not associated with files)
            (when (and (buffer-modified-p)
                       (not ξemacs-buff-p)
                       (not (string-equal major-mode "dired-mode"))
                       (if (equal (buffer-file-name) nil)
                           (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                         t))
              (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
                  (save-buffer)
                (set-buffer-modified-p nil)))
            (when (and (buffer-modified-p)
                       ξorg-p)
              (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
                  (org-edit-src-save)
                (set-buffer-modified-p nil)))

            ;; save to a list of closed buffer
            (when (not (equal buffer-file-name nil))
              (setq xah-recently-closed-buffers
                    (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
              (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
                (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

            ;; close
            (kill-buffer (current-buffer))

            ;; if emacs buffer, switch to a user buffer
            (when (string-match "^*" (buffer-name))
              (next-buffer)
              (let ((i 0))
                (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
                  (setq i (1+ i)) (next-buffer)))))))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop xah-recently-closed-buffers))))

(defun xah-open-recently-closed ()
  "Open recently closed file."
  (interactive)
  (find-file
   (helm :sources (helm-build-sync-source "recently-closed-files"
                    :candidates (mapcar 'cdr
                                          xah-recently-closed-buffers))
         :buffer "*helm recently close files*")))

(defun xah-list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let ((buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer buf)
    (mapc (lambda (f) (insert (cdr f) "\n"))
          xah-recently-closed-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some added functionalities to Hippie Expend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice he-substitute-string (after he-paredit-fix)
  "Remove extra paren when expanding line in Paredit"
  (when (and paredit-mode
             (equal (substring str -1) ")"))
    (backward-delete-char 1)
    (forward-char)))

(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (he-flexible-abbrev-collect he-search-string))))

  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))

  (cond ((null he-expand-list)
         (if old
             (he-reset-string))
         nil)
        (t
         (he-substitute-string (car he-expand-list))
         (setq he-expand-list (cdr he-expand-list))
         t)))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'symbol) collection))))
    collection))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
          "\\w*-*" "\\b"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading buffer-related functions")
(provide 'ee:functions-buffer)
