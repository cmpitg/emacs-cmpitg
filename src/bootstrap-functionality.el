;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2021-2022 Ha-Duong Nguyen (@cmpitg)
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

;; TODO: Use this one in main config, remove duplicated definition of
;; functions & configuration

(require 'rmacs:bootstrap-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential internal libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'color)
  (require 'windmove)
  (require 'thingatpt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enables all disabled and reports those
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ref: https://www.emacswiki.org/emacs/DisabledCommands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~enable-disabled-commands ()
  "Enable all disabled commands."
  (interactive)
  (message "--- Commands that were disabled ---")
  (mapatoms
   #'(lambda (symbol)
       (when (get symbol 'disabled)
         (put symbol 'disabled nil)
         (message "%s" (prin1-to-string symbol)))))
  (message "--- End: Commands that were disabled ---"))
(add-hook 'emacs-startup-hook #'~enable-disabled-commands)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't follow symlink, use the path as-is
(setq vc-follow-symlinks nil)
(setq find-file-visit-truename nil)

;; 3 lines at a time normally, 5 lines at a time with shift
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll 3 lines at a time
(setq scroll-step 3)
(setq scroll-conservatively 10000)

;; Use system font by default
(setq font-use-system-font t)
;; But do customize font
(defun ~set-gui-font ()
  (interactive)
  (let ((font (format "%s-11" (~get-default-monospace-font)))
        (variable-pitch-font (~get-default-font)))
    (set-frame-font font nil t)
    (add-to-list 'default-frame-alist
                 `(font . ,font))
    (custom-theme-set-faces
     'user
     '(variable-pitch ((t (:family ,variable-pitch-font :height 110))))
     '(fixed-pitch ((t (:family ,font :height 110)))))))
(defun ~set-org-fonts ()
  (interactive)
  (let* ((monospace-font `(:font ,(~get-default-monospace-font)))
         (variable-font `(:font ,(~get-default-font)))
         (base-font-color (face-foreground 'default nil 'default))
         (headline `(:inherit default :weight bold :foreground ,base-font-color))
         (base-height 1.5))
    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-font :height ,base-height))))
                            `(org-level-7 ((t (,@headline ,@variable-font :height ,base-height))))
                            `(org-level-6 ((t (,@headline ,@variable-font :height ,base-height))))
                            `(org-level-5 ((t (,@headline ,@variable-font :height ,base-height))))
                            `(org-level-4 ((t (,@headline ,@variable-font :height ,(* base-height 1.1)))))
                            `(org-level-3 ((t (,@headline ,@variable-font :height ,(* base-height 1.15)))))
                            `(org-level-2 ((t (,@headline ,@variable-font :height ,(* base-height 1.2)))))
                            `(org-level-1 ((t (,@headline ,@variable-font :height ,(* base-height 1.25)))))
                            `(org-document-title ((t (,@headline ,@variable-font :height 1.3 :underline nil)))))))
;; Order matters here
(add-hook 'window-setup-hook #'~set-org-fonts)
(add-hook 'window-setup-hook #'~set-gui-font)

;; Set line spacing
(setq-default line-spacing 2)

;; Custom unique naming method
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Disable Tramp autosave
(setq tramp-auto-save-directory "/tmp/")

;; Don't let the cursor go into minibuffer prompt
;; Ref: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Use the same clipboard with X
(setq select-enable-clipboard t)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;; Always suggest keybindings
(setq suggest-key-bindings t)

;; More tolerable stack
(setq max-lisp-eval-depth 15000)
(setq max-specpdl-size    15000)

;; Don't change case when replacing
(setq-default case-replace nil)

;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Maximum number of ring items to store
(setq mark-ring-max 512)

;; yes/no questions become y/n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backspace and Del delete selection, except in paredit-mode
(delete-selection-mode 1)

;; Set tab width
(setq tab-width 4)

;; Default tab-size for C
(setq-default c-basic-offset 4)

;; Expand tabs to spaces
(setq-default indent-tabs-mode nil)

;; Subword should include camelCase notation
(global-subword-mode 1)
(diminish 'subword-mode)

;; Hide the toolbar
(tool-bar-mode -1)

;; Show tooltips in the echo area
(tooltip-mode -1)

;; Scroll bar comes in handy with mouse usage
(set-scroll-bar-mode 'left)
;; Not anymore
;; (scroll-bar-mode -1)

;; No menu bar, more screen estate
(menu-bar-mode -1)
;; (menu-bar-mode 1)

;; Don't display time
(display-time-mode -1)

;; Don't show the battery indicator
(display-battery-mode -1)

;; Turn off welcome message
(setq inhibit-startup-message t)

;; Display the size of the buffer
(size-indication-mode 1)

;;; Don't blink the cursor
(blink-cursor-mode -1)

;; Window splitting preferences
;; Vertical split
;; (setq split-width-threshold nil)
;; Horizontal split
;; (setq split-width-threshold 1)

;; Change cursor type
;; (set-default 'cursor-type 'hbar)
;; (set-default 'cursor-type 'box)
(set-default 'cursor-type 'bar)

;; Show matching parentheses
(show-paren-mode 1)

;; Set printing type
(setq ps-paper-type 'a4)

;; Disable backup file
(setq make-backup-files nil)

;; Show column number
(column-number-mode 1)

;; Turn on the search-highlighting
(setq search-highlight 1)

;; Case-insensitive searching
(setq-default case-fold-search t)
(setq case-fold-search t)

;; Ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; Dim the ignored part of the file name
(file-name-shadow-mode 1)

;; Minibuffer window expands vertically as necessary to hold the text
;; that you put in the minibuffer
(setq resize-mini-windows t)

;; By default, font-lock mode is off
(global-font-lock-mode -1)
;; (setq font-lock-maximum-size nil)

;; Diminish auto-revert-mode in the mode line
(diminish 'auto-revert-mode)

;; Don't highlight the current line
(hl-line-mode -1)

;; Focus follows mouse
(setq mouse-autoselect-window t)
;; (setq focus-follows-mouse t)
(setq focus-follows-mouse 'auto-raise)

;; Set frame title
(let ((title-format
       `(,(format "Rmacs@%s || %s" (system-name) (or server-name :minimal))
         ;; " \u262f "
         ;; " ☯ "
         " "
         (buffer-file-name "%f"
                           (dired-directory dired-directory "%b")))))
  (setq-default frame-title-format title-format)
  (setq-default icon-title-format title-format)
  (setq frame-title-format title-format)
  (setq icon-title-format title-format))

;; Make window combinations resize proportionally
(setq window-combination-resize t)

;; Allow text drap-and-drop with mouse
(custom-set-variables `(mouse-drag-and-drop-region t))

;; Basic ido setup
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-virtual-buffers t)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard behaviors for C-x, C-c, C-v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ref: https://www.emacswiki.org/emacs/CuaMode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't rebind <C-return>, must set before enabling CUA
(setq cua-rectangle-mark-key "")
(cua-mode 1)
;; Don't tabify after rectangle command
(setq cua-auto-tabify-rectangles nil)
;; No region when it's not highlighted
(transient-mark-mode 1)
;; Don't keep region after copying
(setq cua-keep-region-after-copy nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Menu
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ~get-context-menu ()
    "Returns a list to build a context menu."
    `(""
      ["Open (external)" ~open-externally]
      ["Open dir in term emu" ~open-dir-in-term-emu (~is-selecting?)]
      ["Open term emu" ~open-term-emu]
      ["--" ignore]
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
      ["Exec line" ~execute-line]
      ["--" ignore]
      ["Eval last sexp or region" ~eval-last-sexp-or-region]
      ;; TODO: Include buffer list, new temp buffer, ...
      ["--" ignore]
      ["Undo" undo-tree-undo t]
      ["Redo" undo-tree-redo t]
      ["--" ignore]))

  (defun ~popup-context-menu ()
    "Pops up the context menu."
    (interactive)
    (popup-menu (~get-context-menu)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Editing
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defalias '~is-selecting? 'use-region-p
    "Determines if current there is a selection/active region.")

  (defun ~get-selection ()
    "Gets the currently selected text."
    (if (~is-selecting?)
        (buffer-substring (region-beginning) (region-end))
      ""))

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
                                        (~split-string "\n"))))
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

  (defun ~activate-selection (pos-1 pos-2)
    "Activates a selection, also visually, then leaves the point at
`pos-2'."
    (interactive)
    (set-mark pos-1)
    (goto-char pos-2)
    (activate-mark))
  (defun ~get-secondary-selection ()
    "Gets the secondary selection (activated with `M-Mouse-1' by
default)."
    (interactive)
    (gui-get-selection 'SECONDARY))

  (defun ~to-bol-dwim ()
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
    "Moves up `N-LINES'."
    (interactive)
    (forward-line (- n-lines)))

  (cl-defun ~next-line+ (&optional (n-lines 5))
    "Moves down `N-LINES'."
    (interactive)
    (forward-line n-lines))

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

  ;; TODO cleanup
  (defun ~duplicate-line-or-region (&optional n)
    "Duplicate current line, or region if active.

With argument N, make N copies.
With negative N, comment out original line and use the absolute value.

Source: http://stackoverflow.com/a/4717026/219881"
    (interactive "*p")
    (let ((use-region (use-region-p)))
      (save-excursion
        (let ((text (if use-region      ; Get region if active, otherwise line
                        (buffer-substring (region-beginning) (region-end))
                      (prog1 (thing-at-point 'line)
                        (end-of-line)
                        (if (< 0 (forward-line 1)) ; Go to beginning of next
                                        ; line, or make a new one
                            (newline))))))
          (dotimes (_ (abs (or n 1)))   ; Insert N times, or once if not
                                        ; specified
            (insert text))))
      (if use-region nil            ; Only if we're working with a line (not a
                                        ; region)
        (let ((pos (- (point) (line-beginning-position)))) ; Save column
          (if (> 0 n)                   ; Comment out original with negative
                                        ; arg
              (comment-region (line-beginning-position) (line-end-position)))
          (forward-line 1)
          (forward-char pos)))))

  (defun ~join-with-next-line ()
    "Joins next line with the current line."
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

  (defun ~delete-line ()
    "Deletes the current line."
    (interactive)
    (delete-region (point-at-bol) (point-at-eol)))

  (defun ~delete-blank-lines ()
    "Deletes all blank lines at the current position."
    (interactive)
    (delete-blank-lines)
    (when (looking-at (rx bol (0+ space) eol))
      (kill-line)))

  (cl-defun ~search-buffer-interactively ()
    "Searches the current buffer interactively."
    (interactive)
    (swiper (~get-selection)))

  (defun ~format-json ()
    "Formats current selection as JSON.  Requires jq."
    (interactive)
    (~exec| "jq ."))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Emacs Lisp
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ~identity (x)
    "The identity function."
    x)

  (defmacro ~comment (&rest body)
    "Ignores the body."
    nil)

  (defun ~current-dir ()
    "Current directory."
    (or (file-name-directory (or load-file-name buffer-file-name ""))
        ""))

  (defun ~current-file-full-path ()
    "Full path to current file."
    (or (expand-file-name (or buffer-file-name ""))
        ""))

  (defun ~get-current-project-root ()
    "Returns the current project root or current directory."
    (or (ignore-errors (first (last (project-current))))
        default-directory))

  (defun ~expand-path-fully (path)
    "Expands PATH fully."
    (substitute-env-vars (expand-file-name path)))

  (defun ~clean-up-tramp ()
    "Closes all tramp connections and buffers."
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))

  (defun ~find (pred xs)
    "Finds and returns the first element from the list XS that satisfies PRED.
If no element is found, returns nil."
    (cl-loop for x in xs
             when (funcall pred x)
             return x))

  (cl-defun ~set-fn-docstring (symbol new-docstring)
    "Sets docstring for a function."
    (put symbol 'function-documentation new-docstring))

  ;; Ref: https://stackoverflow.com/a/17310748/219881
  (defun ~make-repeatable-fn (cmd)
    "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
    (fset (intern (concat (symbol-name cmd) "---repeat"))
          `(lambda ,(help-function-arglist cmd) ;; arg list
             ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
             ,(interactive-form cmd) ;; interactive form
             ;; see also repeat-message-function
             (setq last-repeatable-command ',cmd)
             (repeat nil)))
    (intern (concat (symbol-name cmd) "---repeat")))

  (cl-defun ~make-repeatable-multi-fn (cmd keybinding-alist)
    "Returns an interned symbol, storing the function that is a
repeatable version of CMD with multiple keybindings."
    (interactive)
    (let ((fn-symb (intern (concat (symbol-name cmd) "---repeat-multi"))))
      (fset fn-symb (~repeatable-multi-fn cmd keybinding-alist))
      (~set-fn-docstring fn-symb (format "A repeatable version of %s with multiple keybindings" cmd))
      fn-symb))

  (cl-defun ~repeatable-multi-fn (cmd keybinding-alist)
    "Returns a lambda as a repeatable version of CMD with multiple
keybinding."
    (interactive)
    #'(lambda ()
        (interactive)
        ;; see also repeat-message-function
        (call-interactively cmd)
        (set-transient-map
         (let ((keymap (make-sparse-keymap)))
           (mapcar #'(lambda (keybinding)
                       (let ((key (car keybinding))
                             (fn (cdr keybinding)))
                         (bind-key key
                                   (~repeatable-multi-fn fn keybinding-alist)
                                   keymap)))
                   keybinding-alist)
           keymap))))

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

  (defun ~insert-full-line-comment ()
    "Inserts a line full of comment characters until `fill-column'
is reached."
    (interactive)
    (let ((comment (string-trim comment-start)))
      (thread-first
        (cl-loop for time from (current-column) upto (1- fill-column) by (length comment)
                 collect comment)
        (string-join "")
        insert)))

  (cl-defun ~insert-file-and-goto-end (filepath)
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
    (cl-multiple-value-bind
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

  (defun ~get-last-sexp ()
    "Returns the last sexp before the current point."
    (interactive)
    (elisp--preceding-sexp))

  (defun ~get-cursor-pos-at-last-mouse-event ()
    "Returns the position of the mouse cursor if the last command
event is a mouse event, or `nil' otherwise."
    (interactive)
    (posn-point (event-end last-command-event)))

  (defun ~ido-M-x ()
    "Calls `EXECUTE-EXTENDED-COMMAND' with ido."
    (interactive)
    (call-interactively
     (intern
      (ido-completing-read
       "M-x "
       (all-completions "" obarray 'commandp)))))

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
      (let ((str (string-trim str)))
        (or (check-file-exists? str)
            (let ((components (~split-string ":" str)))
              (and (= 2 (length components))
                   (check-file-exists? (first components))))))))

  (defun ~try-getting-current-thing ()
    "Returns text from the current context:

- If the last command is a mouse event, go to the point under the
  cursor.

- if the current line is a path, returns it; or

- if the thing-at-point could be retrieved as a symbol, returns
  its string representation; otherwise

- returns the last sexp."
    (interactive)
    (save-excursion
      (if-let (point (~get-cursor-pos-at-last-mouse-event))
          (goto-char point))
      (let ((path (string-trim-right (thing-at-point 'line))))
        (or (and (~file-pattern? path) path)
            (thing-at-point 'symbol)
            (~get-last-sexp)))))

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

  (defun ~eval-last-sexp-or-region ()
    "Evals region if active, or evals last sexpr."
    (interactive)
    (if (~is-selecting?)
        (call-interactively #'~eval-region)
      (call-interactively #'eval-last-sexp)))

  (cl-defun ~gen-filename (&key (separator "-")
                                (word-counter 4))
    "Generates a random file name."
    (string-trim (~exec-sh (list "gen-filename"
                                 (number-to-string word-counter)
                                 separator))))

  (defun ~gen-uuid ()
    "Generates a UUID."
    (string-trim (~exec-sh (list "uuidgen" "-r"))))

  (defun ~split-string (regexp s &optional omit-nulls)
    "Split S into substrings bounded by matches for regexp REGEXP.
If OMIT-NULLS is non-nil, zero-length substrings are omitted."
    (declare (side-effect-free t))
    (save-match-data (split-string s regexp omit-nulls)))

  (defun ~string-matches? (regexp s &optional start)
    "Determines of REGEXP match S."
    (declare (side-effect-free t))
    (not (null (string-match-p regexp s start))))

  (defun ~string-split-up-to (regex s n &optional omit-nulls)
    "Splits string S up to N times into substrings.

If OMIT-NULLS is non-nil, zero-length substrings are omitted.

Based on the implementation of `s-split-up-to' from s.el."
    (declare (side-effect-free t))
    (save-match-data
      (let ((res nil))
        (with-temp-buffer
          (cl-labels ((check-and-record-substr
                       (substr)
                       (unless (and omit-nulls (equal substr ""))
                         (push substr res))))
            (insert s)
            (let ((current-point (goto-char (point-min))))
              (while (and (> n 0)
                          (re-search-forward regex nil t))
                (check-and-record-substr (buffer-substring current-point (match-beginning 0)))
                (setq current-point (goto-char (match-end 0)))
                (cl-decf n))
              (check-and-record-substr (buffer-substring current-point (point-max))))))
        (nreverse res))))

  (defun ~string-match (regexp s &optional start)
    "Returns the list of the whole matching string and one string for each matched regexp.  Returns nil if there is no match.

When START is non-nil the search will start at that index.

Based on the implementation of `s-match' in s.el."
    (declare (side-effect-free t))
    (save-match-data
      (if (string-match regexp s start)
          (let ((match-list (match-data))
                res)
            (while match-list
              (let* ((beg (car match-list))
                     (end (cadr match-list))
                     (substrs (if (and beg end) (substring s beg end) nil)))
                (setq res (cons substrs res))
                (setq match-list (cddr match-list))))
            (nreverse res)))))

  (defun ~current-line-continues? ()
    "Determines if the current line has a continuation marker."
    (thread-last (thing-at-point 'line)
                 string-trim
                 (~string-matches? (rx "\\" (0+ space) eol))))

  (defun ~previous-line-continues? ()
    "Determines if the previous line has a continuation marker."
    (unless (zerop (line-number-at-pos))
      (save-mark-and-excursion
        (forward-line -1)
        (~current-line-continues?))))

  (cl-defun ~shorten-string (str max-length &optional (ellipsis "…") (cut-beginning? t))
    "Shortens a string, making sure its length does not exceed
MAX-LENGTH by truncating and prefixing it with ELLIPSIS if
necessary."
    (let ((actual-length (length str)))
      (if (> actual-length max-length)
          (if cut-beginning?
              (concat ellipsis (substring str (+ (- actual-length
                                                    max-length)
                                                 (length ellipsis))))
            (concat (substring str (+ (- actual-length
                                         max-length)
                                      (length ellipsis)))
                    ellipsis)
            )
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Window & Frame
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ~find-file-new-frame (path &optional wildcards)
    "Calls `find-file' in a new frame."
    (let ((frame (make-frame)))
      (select-frame frame)
      (find-file path wildcards)))

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
    (cl-loop for window being the windows
             unless (window-dedicated-p window)
             count window))

  (cl-defun ~count-windows ()
    "Counts the number of windows in the current frame."
    (cl-loop for window being the windows
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

  (defun ~split-window-right ()
    (interactive)
    (~split-window 'right))

  (defun ~split-window-below ()
    (interactive)
    (~split-window 'below))

  (cl-defun ~get-non-minibuffer-window-in-dir (dir)
    "Gets the next non-minibuffer in a direction.
If the found window is the mini-buffer, returns `nil'."
    (require 'windmove)
    (let ((window (windmove-find-other-window dir)))
      (unless (minibufferp (window-buffer window))
        window)))

  (defun ~get-current-monitor-workarea (&optional frame)
    "Returns the current position and size of the workarea for the
current monitor in the format of (X Y WIDTH HEIGHT)"
    (alist-get 'workarea (frame-monitor-attributes frame)))

  (defun ~centralize-mouse-position ()
    "Centralizes mouse position in the current window."
    (interactive)
    (unless (minibufferp (current-buffer))
      (let ((frame (selected-frame)))
        (destructuring-bind (x1 y1 x2 y2) (window-edges)
          (set-mouse-position frame
                              (+ x1 (/ (window-body-width) 2))
                              (+ y1 (/ (window-body-height) 2)))))))

  (defun ~auto-pos-mouse-position ()
    "Automatically position mouse in a sensible way."
    (interactive)
    (when (eq (current-buffer) (window-buffer (selected-window)))
      (unless (minibufferp (current-buffer))
        (let ((frame (selected-frame)))
          (destructuring-bind (x1 y1 x2 y2) (window-edges)
            (set-mouse-position frame (+ x1 1) y1))))))

  (cl-defun ~center-frame (width
                           height
                           &key
                           (frame (selected-frame)))
    "Centers a frame.  WIDTH and HEIGHT are in pixels."
    (set-frame-size frame width height t)
    (destructuring-bind (x y screen-width screen-height) (~get-current-monitor-workarea frame)
      (let* ((desired-x (+ x (/ (- screen-width width) 2)))
             (desired-y (+ y (/ (- screen-height height) 2))))
        (set-frame-position frame desired-x desired-y))))

  (cl-defun ~center-frame-percent (width%
                                   height%
                                   &key
                                   (frame (selected-frame)))
    "Centers a frame.  WIDTH% and HEIGHT% are integers
corresponding to the percentage of the width & height with
regards to the current screen."
    (destructuring-bind (x y screen-width screen-height) (~get-current-monitor-workarea frame)
      (let* ((width (* (/ screen-width 100) width%))
             (height (* (/ screen-height 100) height%))
             (desired-x (+ x (/ (- screen-width width) 2)))
             (desired-y (+ y (/ (- screen-height height) 2))))
        (set-frame-size frame width height t)
        (set-frame-position frame desired-x desired-y))))

  (cl-defun ~center-frame-in-chars (width-in-chars
                                    height-in-chars
                                    &key
                                    (frame (selected-frame)))
    "Centers a frame with the width & height dimensions in
characters."
    (set-frame-size frame width-in-chars height-in-chars)
    (let* ((width (frame-pixel-width frame))
           (height (frame-pixel-height frame)))
      (~center-frame width height :frame frame)))

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

  (cl-defun ~transpose-windows (&optional (window-selection-fn #'~get-next-non-dedicated-window))
    "Transposes the current window with the next one."
    (interactive)
    (when (window-dedicated-p (selected-window))
      (error "Current window is dedicated, cannot transpose"))
    (let ((windows (cl-loop for window in (window-list)
                            when (window-dedicated-p window)
                            collect window)))
      (let* ((current-window (selected-window))
             (current-buffer (window-buffer))
             (next-window (apply window-selection-fn current-window nil))
             (next-buffer (window-buffer next-window)))
        (set-window-buffer next-window current-buffer)
        (set-window-buffer current-window next-buffer))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; File & buffer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defalias '~switch-buffer 'switch-to-buffer)

  (defun ~file-glob (pattern &optional path)
    "Globs PATTERN in PATH."
    (file-expand-wildcards
     (concat (or path default-directory) pattern)))

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

  (defun ~rename-current-file (&optional new-name)
    "Renames the current file."
    (interactive "FNew name: ")
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

  (defun ~save-buffer-as (path)
    "Saves current file as."
    (interactive "FPath: ")
    (unless (string-empty-p path)
      (write-file path t)))

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

  (defalias '~switch-to-last-buffer 'mode-line-other-buffer
    "Switches to the most recently visited buffer.")

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
    (cl-loop for buf in (buffer-list)
             for file-name = (buffer-file-name buf)
             when (and file-name (file-exists-p file-name))
             do (ignore-errors (with-current-buffer buf
                                 (message "Reverting %s" file-name)
                                 (revert-buffer t t)))))

  (defun ~visit-file (path)
    "Visits a file without opening it and returns the buffer name."
    (buffer-name (find-file-noselect path)))

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

  (cl-defun ~open-externally (&optional text)
    "Opens TEXT externally."
    (interactive)
    (defvar *~open-externally-history* (list))
    (let* ((text (if (~is-selecting?)
                     (~get-selection)
                   (read-string "Text: " (thing-at-point 'filename) '*~open-externally-history*)))
           (expanded (~expand-path-fully (string-trim text))))
      (~run-process (concat "xdg-open" " " (shell-quote-argument expanded))
                    :async t)))

  (cl-defun ~open-dir-in-term-emu (&optional dir-path)
    "Opens a directory path in a terminal emulator."
    (interactive)
    (defvar *~open-dir-in-term-history* (list))
    (let* ((text (if (~is-selecting?)
                     (~get-selection)
                   (read-string "Text: " (thing-at-point 'filename) '*~open-dir-in-term-history*)))
           (expanded (~expand-path-fully (string-trim text))))
      (~run-process (concat "with-workdir" " " (shell-quote-argument expanded) " " "x-terminal-emulator")
                    :async t)))

  (cl-defun ~open-term-emu ()
    "Opens a terminal emulator."
    (interactive)
    (~run-process "x-terminal-emulator" :async t))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tracking recently closed files
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; History management
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process/Execution
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ~get-process-output (process)
    "Gets the output for a managed process."
    (with-current-buffer (process-buffer process)
      (buffer-string)))

  (cl-defun ~run-process (command &key (async t))
    "Runs an external process.  If ASYNC is non-`nil' the process is
not terminated when Emacs exits and the output is discarded;
otherwise, both output from stdout and stderr are directed to the
buffer whose name is after COMMAND.  COMMAND is executed via the
current user shell, define by the SHELL environment variable.
Note that COMMAND is not automatically quoted and should be
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
    (lexical-let ((command (cl-typecase command
                             (string command)
                             (list (string-join (cl-loop for arg in command
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
    `(funcall ,(cl-loop for command in (reverse commands)
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
    `(~exec-pipe-async ,@(cl-loop for command in commands
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

  (defun ~dispatch-action (&rest args)
    "Dispatches action based on args.  Ignore output."
    (interactive)
    (message "Dispatching action: %s; Workdir: %s" args (pwd))
    (eval `(~exec-|-async ("setsid" "--fork" "dispatch-action" ,@args))))

  (defun ~run-current-file-in-tmux-then-pause ()
    "Runs current file in a terminal emulator, pauses after finished."
    (interactive)
    (let ((path (buffer-file-name)))
      (when (or (null path) (string-empty-p path))
        (error "Current buffer must be a file"))
      (~dispatch-action "!! " path)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Context-menu execution
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (~comment
   (~execute "(+ 1 1)")
   (~execute "(message-box \"Hello world!\")")
   (~execute "message-box \"Hello world!\"")
   (~execute "+ 1 1")
   (~execute "$ ls -1"))

  (cl-defun ~execute-line ()
    "Executes current line with `~execute'."
    (interactive)
    (~execute (string-trim (thing-at-point 'line t))))

  (cl-defun ~execute-text-prompt ()
    "Prompts for text and executes it with `~execute'."
    (interactive)
    (defvar *~execute-text-prompt-hist* (list))
    (let ((text (read-from-minibuffer "Text: " nil nil nil '*~execute-text-prompt-hist*)))
      (~execute text)))

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; High-level functions for better UX
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ~turn-on-soft-wrapping ()
    "Turns on soft-wrapping."
    (interactive)
    (turn-off-auto-fill)
    (turn-on-visual-line-mode)
    (when (featurep 'visual-fill-column)
      (turn-on-visual-fill-column-mode)))

  (defun ~turn-off-soft-wrapping ()
    "Turns off soft-wrapping."
    (interactive)
    (visual-line-mode -1)
    (when (featurep 'visual-fill-column)
      (visual-fill-column-mode -1)))

  (defun ~toggle-soft-wrapping ()
    "Toggles on soft-wrapping mode."
    (interactive)
    (let* ((soft-wrapped? (if (featurep 'visual-fill-column)
                              visual-fill-column-mode
                            visual-line-mode)))
      (if soft-wrapped?
          (~turn-off-soft-wrapping)
        (~turn-on-soft-wrapping))))

  (cl-defun ~split-window (&optional (side 'right))
    "Splits the current window & switch to the new window."
    (interactive)
    (when-let (window (split-window (selected-window) nil side nil))
      (select-window window)
      (call-interactively #'~switch-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(with-eval-after-load "org"
  (require 'org-archive)
  (defun ~org-set-fold-entry ()
    (interactive)
    (org-set-property "~fold?" "true"))

  (defun ~org-is-entry-folded? (&optional pom)
    (or (string= (org-entry-get pom "~fold?") "true")
        (ignore-errors (org-entry-is-done-p))))

  (defun ~org-fold-entry ()
    (interactive)
    (org-flag-subtree t))

  (defun ~org-unfold-entry ()
    (interactive)
    (org-flag-subtree nil))

  (defun ~org-unfold-all ()
    (interactive)
    (org-show-all '(headings block)))

  (defun ~org-refresh-fold-state ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (when (~org-is-entry-folded?)
          (~org-fold-entry))
        (~org-to-next-entry))))

  (defalias '~org-to-next-entry #'outline-next-heading)
  (defalias '~org-to-prev-entry #'outline-previous-heading)

  (defun ~my/org-mode-setup ()
    ;; Don't open link with <mouse-1>
    (setq-local mouse-1-click-follows-link nil)

    ;; Darken background of code block
    (require 'color)
    (set-face-attribute 'org-block nil :background
                        (color-darken-name (face-attribute 'default :background) 3))
    (variable-pitch-mode 1)
    (bind-key "<M-return>"        #'org-meta-return           org-mode-map)
    (bind-key "<S-return>"        #'~execute-line             org-mode-map)
    (bind-key "<C-return>"        #'~eval-last-sexp-or-region org-mode-map)
    (bind-key "C-<tab>"           #'iflipb-next-buffer        org-mode-map)
    (bind-key "C-S-<tab>"         #'iflipb-previous-buffer    org-mode-map)
    (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer    org-mode-map)
    (bind-key "C-e"               nil                         org-mode-map)
    (~org-refresh-fold-state)
    (font-lock-mode t))
  (add-hook 'org-mode-hook #'~my/org-mode-setup)

  ;; Add timestamp when an item is done
  (setq org-log-done 'time)

  (setq org-agenda-files (thread-last (file-name-directory *toolbox-path*)
                                      (~file-glob "*.org")))

  ;; Indent visually by default
  (setq org-startup-indented t)

  ;; Smart editing of invisible text
  (setq org-catch-invisible-edits 'smart)

  ;; TAB-cycle plain list as children of their heading parent
  (setq org-cycle-include-plain-lists 'integrate)

  ;; Hide the emphasis markers in font-lock-mode
  ;; (setq org-hide-emphasis-markers t)
  (setq org-hide-emphasis-markers nil)

  ;; Continuation symbol
  (setq org-ellipsis " ↩")

  ;; Don't split line by default
  (setq org-M-RET-may-split-line nil)

  ;; Don't fontify code block by default
  (setq org-src-fontify-natively nil)

  ;; Preserve indentation in org-src
  (setq org-src-preserve-indentation t)

  ;; Enable shift-selection all the time
  (setq org-support-shift-select 'always)

  ;; Logical TODO & checkbox dependencies
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  ;; No folding by default
  ;; Ref: https://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
  ;; Of per file: #+STARTUP: all
  (setq org-startup-folded nil)

  ;; Modules that should be loaded with org
  (dolist (module '(org-crypt
                    org-habit
                    ;; org-bookmark
                    ;; org-eshell
                    ))
    (add-to-list 'org-modules module))

  ;; org-babel
  (setq org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)
          (R . t)
          (clojure . t)
          (shell . t)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3")

  (setq-default initial-major-mode 'org-mode)
  (setq-default major-mode 'org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directory browsing with Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dired)
(with-eval-after-load 'dired
  (custom-set-variables '(dired-maybe-use-globstar t)
                        '(dired-listing-switches "-labhFgG --group-directories-first"))

  (defun ~my/setup-dired-mode ()
    (interactive)
    (unbind-key "<mouse-2>" dired-mode-map)
    (setq-local mouse-1-click-follows-link nil))

  (add-hook 'dired-mode-hook #'~my/setup-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eshell)
(require 'em-smart)

(defun ~eshell-dir (dir)
  "Starts Eshell in a directory.  If Eshell has already been
started, change the directory."
  (interactive "DDirectory: ")
  (cond
   ((get-buffer "*eshell*")
    (with-current-buffer "*eshell*"
      (cd dir)
      (eshell-emit-prompt))
    (switch-to-buffer "*eshell*"))
   (t
    (cd dir)
    (eshell))))

(defun ~eshell-current-project-dir ()
  "Starts Eshell in the current project directory or the current
directory."
  (interactive)
  (~eshell-dir (~get-current-project-root)))

(defun ~eshell-quit ()
  "Quits Eshell when current command is empty."
  (interactive)
  (insert "exit")exit
  (eshell-send-input))

;; (defun ~my/eshell-prompt-function ()
;;   (format " - %s %s@%s %s -\n%s "
;;           (format-time-string "%H:%M:%S" (current-time))
;;           (user-login-name)
;;           (system-name)
;;           (eshell/pwd)
;;           (if (zerop (user-uid)) "#" "$")))
;; (defun ~my/eshell-prompt-function ()
;;   (format " - %s %s@%s %s -\n"
;;           (format-time-string "%H:%M:%S" (current-time))
;;           (user-login-name)
;;           (system-name)
;;           (eshell/pwd)))
;; (setq eshell-prompt-function #'~my/eshell-prompt-function)
;; (setq eshell-prompt-regexp (rx bol (or "#" "$") " "))

(defun ~my/eshell-maybe-bol ()
  "Goes to beginning of command line first, then beginning of
line in Eshell."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (when (= p (point))
      (beginning-of-line))))

(defun ~init-eshell ()
  "Initializes Eshell.  Call this only after Eshell has been fully setup, e.g. in `ESHELL-MODE-HOOK'."
  (interactive)
  (eshell-smart-initialize)
  (add-to-list 'eshell-visual-commands "mutt")
  (add-to-list 'eshell-visual-subcommands `("git" "log" "diff" "show")))

(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "<menu>" 'nil)
(bind-key "s-SPC" 'nil)
(bind-key "M-SPC" 'nil)

;;
;; Basic editing
;;

;; Movements
(bind-key "s-c" #'previous-line)
(bind-key "s-t" #'next-line)
(bind-key "s-h" #'backward-char)
(bind-key "s-n" #'forward-char)
(bind-key "s-d" #'~to-bol-dwim)
(bind-key "s-D" #'move-end-of-line)
(bind-key "M-s-c" #'~previous-line+)
(bind-key "M-s-t" #'~next-line+)
(bind-key "s-H" #'beginning-of-buffer)
(bind-key "s-N" #'end-of-buffer)
(bind-key "s-g" #'backward-word)
(bind-key "s-G" #'backward-sexp)
(bind-key "s-r" #'forward-word)
(bind-key "s-R" #'forward-sexp)
(bind-key "s-l" #'goto-line)

;; Deletion
(bind-key "s-u" #'delete-char)
(bind-key "s-e" #'backward-delete-char)
(bind-key "s-p" #'kill-word)
(bind-key "s-." #'backward-kill-word)
(bind-key "<C-delete>" #'delete-region)

;; Searching
(bind-key "s-f" #'query-replace-regexp)
(bind-key "s-F" #'query-replace)
(bind-key "s-s" #'~search-buffer-interactively)

;; Text processing
(bind-key "RET" #'~electrify-return-if-match)
(bind-key "s--" #'comment-or-uncomment-region)
(bind-key "s-w" #'whitespace-cleanup)
(bind-key "s-@" #'~duplicate-line-or-region)
(bind-key "s-&" #'~join-with-next-line)
(bind-key "s-b" #'pop-to-mark-command)
(bind-key "s-'" #'undo)
(bind-key "s-\"" #'undo-redo)

;; Buffer management
(bind-key "C-w" #'kill-current-buffer)
(bind-key "C-S-t" #'~undo-killed-buffers)

;; Emacs Lisp
(bind-key "<C-return>" #'~eval-last-sexp-or-region)
(bind-key "<M-return>" #'eval-defun)
(bind-key "<s-RET>" #'~execute)
(bind-key "<S-RET>" #'~execute-line)
(bind-key "<s-return>" #'~execute)
(bind-key "<S-return>" #'~execute-line)
(bind-key "<C-down-mouse-1>" nil)
(bind-key "<down-mouse-2>" nil)
(bind-key "<mouse-2>" #'~execute)
(bind-key "<mouse-3>" #'~popup-context-menu)
(bind-key "<C-down-mouse-3>" #'~popup-context-menu)

;; Window
(with-eval-after-load "windmove"
  (bind-key "<M-left>" #'windmove-left)
  (bind-key "<M-right>" #'windmove-right)
  (bind-key "<M-up>" #'windmove-up)
  (bind-key "<M-down>" #'windmove-down))

;; Function keys & other convenient bindings
(bind-key "<f2>" #'save-buffer)
(bind-key "<f3>" #'find-file)
(bind-key "<C-f4>" #'kill-current-buffer)
(bind-key "<f8>" #'~switch-buffer)
;; BUG: Command history not recorded
;; (bind-key "<f12>" #'~ido-M-x)
(bind-key "<f12>" #'execute-extended-command)

;; Header line
(bind-key "<header-line> <mouse-3>" #'~header-line-execute)
(bind-key "<header-line> <M-mouse-3>" #'~header-line-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "M-SPC M-SPC" #'execute-extended-command)
(bind-key "M-SPC SPC" #'execute-extended-command)

;; Buffer
(bind-key "M-SPC b r" #'revert-buffer)
(bind-key "M-SPC b n" #'~new-buffer)
(bind-key "M-SPC b m" #'mark-whole-buffer)
(bind-key "M-SPC b l" #'~show-buffer-chooser)
(bind-key "M-SPC b b" #'list-buffers)
(bind-key "M-SPC b k k" #'kill-current-buffer)
(bind-key "M-SPC b k b" #'kill-buffer)

(bind-key "M-SPC w s r" #'(lambda () (interactive) (~split-window 'right)))
(bind-key "M-SPC w s b" #'(lambda () (interactive) (~split-window 'below)))
(bind-key "M-SPC w k" #'delete-window)
(bind-key "M-SPC w o" #'~one-window)
(bind-key "M-SPC w t" #'~transpose-windows)

;; Visit
(bind-key "M-SPC v n" #'~open-project-notes)
(bind-key "M-SPC v b" #'~open-project-toolbox)
(bind-key "M-SPC v t" #'~open-toolbox)
(bind-key "M-SPC v p p" #'projectile-switch-project)

;; File
(bind-key "M-SPC f d" #'~delete-current-file)
(bind-key "M-SPC f r" #'~rename-current-file)
(bind-key "M-SPC f s" #'save-buffer)
(bind-key "M-SPC f a" #'~save-buffer-as)
(with-eval-after-load "counsel"
  (bind-key "M-SPC f o" #'counsel-find-file))
(with-eval-after-load "projectile"
  (bind-key "M-SPC f f" #'projectile-find-file))
(bind-key "M-SPC f e" #'~find-files-current-dir)
(bind-key "M-SPC f i" #'~find-files-current-dir-not-ignoring)
(bind-key "M-SPC f m" #'~open-current-file-as-admin)
(bind-key "M-SPC f c" #'~copy-file-name-to-clipboard)
(bind-key "M-SPC f p" #'~copy-pos-to-clipboard)
(bind-key "M-SPC f t" #'~choose-path-and-act)

;; Jumping
(with-eval-after-load "ace-jump"
  (bind-key "M-SPC j a" #'ace-jump-mode)
  (bind-key "M-SPC j l" #'ace-jump-line-mode))
;; (with-eval-after-load "dumb-jump"
;;   ("'" #'dumb-jump-go-other-window "Try jumping to def (other window)")
;;   ("." #'dumb-jump-go "Try jumping to def")
;;   ("," #'dumb-jump-back "Jump back")
;;   (";" #'dumb-jump-quick-look "Peek"))
(with-eval-after-load "smart-jump"
  (bind-key "M-SPC j ." #'smart-jump-go)
  (bind-key "M-SPC j ," #'smart-jump-back))
(bind-key "M-SPC j j" (~make-repeatable-fn #'~goto-next-line-matching-marker))
(bind-key "M-SPC j k" (~make-repeatable-fn #'~goto-prev-line-matching-marker))
(with-eval-after-load "point-pos"
  (bind-key "M-SPC j s" #'point-pos-save)
  (bind-key "M-SPC j g" #'point-pos-goto)
  (bind-key "M-SPC j n" (~make-repeatable-fn #'point-pos-next))
  (bind-key "M-SPC j p" (~make-repeatable-fn #'point-pos-previous)))
(with-eval-after-load "counsel"
  (bind-key "M-SPC j i" #'counsel-imenu))

;; Cursor
(with-eval-after-load "multiple-cursors"
  (bind-key "M-SPC c l" #'mc/edit-lines)
  (bind-key "M-SPC c n" #'mc/mark-next-like-this)
  (bind-key "M-SPC c p" #'mc/mark-previous-like-this)
  (bind-key "M-SPC c a" #'mc/mark-all-in-region))

;; Emacs Lisp
(bind-key "M-SPC l e e" #'~eval-last-sexp-or-region)
(bind-key "M-SPC l e p" #'pp-eval-last-sexp)
(bind-key "M-SPC l e w" #'~eval-then-replace-region-or-last-sexp)
(bind-key "M-SPC l e x" #'eval-expression)
(bind-key "M-SPC l e r" #'eval-region)
(bind-key "M-SPC l e b" #'eval-buffer)
(bind-key "M-SPC l h f" #'describe-function)
(bind-key "M-SPC l h v" #'describe-variable)
(bind-key "M-SPC l h k" #'describe-key)
(bind-key "M-SPC l h ." #'find-function)
(bind-key "M-SPC l l" #'find-library)

;; Org
(with-eval-after-load "org"
  (let ((repeatable-keybindings `(("i" . org-insert-heading)
                                  ("I" . org-insert-item)
                                  ("TAB" . org-cycle)
                                  ("c" . org-ctrl-c-ctrl-c)
                                  (">" . org-metaright)
                                  ("<" . org-metaleft)
                                  ("r >" . org-shiftmetaright)
                                  ("r <" . org-shiftmetaleft)
                                  ("P" . org-move-subtree-up)
                                  ("N" . org-move-subtree-down)

                                  ("j" . org-next-visible-heading)
                                  ("k" . org-previous-visible-heading)
                                  ("J" . org-forward-heading-same-level)
                                  ("K" . org-backward-heading-same-level)

                                  ("t a" . org-ctrl-c-ctrl-c)

                                  ("o i" . org-insert-todo-heading)
                                  ("o c" . org-todo))))
    ;; Nagivation
    (bind-key "M-SPC o j" (~make-repeatable-multi-fn #'org-next-visible-heading repeatable-keybindings))
    (bind-key "M-SPC o k" (~make-repeatable-multi-fn #'org-previous-visible-heading repeatable-keybindings))
    (bind-key "M-SPC o J" (~make-repeatable-multi-fn #'org-forward-heading-same-level repeatable-keybindings))
    (bind-key "M-SPC o K" (~make-repeatable-multi-fn #'org-backward-heading-same-level repeatable-keybindings))

    ;; List/tree
    (bind-key "M-SPC o a" #'org-archive-subtree)
    (bind-key "M-SPC o >" (~make-repeatable-multi-fn #'org-metaright repeatable-keybindings))
    (bind-key "M-SPC o <" (~make-repeatable-multi-fn #'org-metaleft repeatable-keybindings))
    (bind-key "M-SPC o r >" (~make-repeatable-multi-fn #'org-shiftmetaright repeatable-keybindings))
    (bind-key "M-SPC o r <" (~make-repeatable-multi-fn #'org-shiftmetaleft repeatable-keybindings))
    (bind-key "M-SPC o i" (~make-repeatable-multi-fn #'org-insert-heading repeatable-keybindings))
    (bind-key "M-SPC o I" (~make-repeatable-multi-fn #'org-insert-item repeatable-keybindings))
    (bind-key "M-SPC o TAB" (~make-repeatable-multi-fn #'org-cycle repeatable-keybindings))
    (bind-key "M-SPC o c" (~make-repeatable-multi-fn #'org-ctrl-c-ctrl-c repeatable-keybindings))
    (bind-key "M-SPC o P" (~make-repeatable-multi-fn #'org-move-subtree-up repeatable-keybindings))
    (bind-key "M-SPC o N" (~make-repeatable-multi-fn #'org-move-subtree-down repeatable-keybindings))
    (bind-key "M-SPC o m" #'org-mark-subtree)
    (bind-key "M-SPC o g" #'org-goto)
    (bind-key "M-SPC o /" #'org-sparse-tree)
    (bind-key "M-SPC o s" #'org-insert-structure-template)

    ;; Table
    (bind-key "M-SPC o t a" (~make-repeatable-multi-fn #'org-ctrl-c-ctrl-c repeatable-keybindings))

    ;; Todo
    (bind-key "M-SPC o o i" (~make-repeatable-multi-fn #'org-insert-todo-heading repeatable-keybindings))
    (bind-key "M-SPC o o c" (~make-repeatable-multi-fn #'org-todo repeatable-keybindings))
    (bind-key "M-SPC o o s" #'org-schedule)

    ;; Narrowing
    (bind-key "M-SPC o n s" #'org-narrow-to-subtree)
    (bind-key "M-SPC o n b" #'org-narrow-to-block)
    (bind-key "M-SPC o n w" #'widen)

    ;; Editing
    (bind-key "M-SPC o m" #'org-mark-subtree)))

;; Edit
(bind-key "M-SPC d q r" #'query-replace-regexp)
(bind-key "M-SPC d q q" #'query-replace)
(bind-key "M-SPC d n r" #'narrow-to-region)
(bind-key "M-SPC d n f" #'narrow-to-defun)
(bind-key "M-SPC d n w" #'widen)
(bind-key "M-SPC d a a" #'align)
(bind-key "M-SPC d a c" #'align-current)
(bind-key "M-SPC d a r" #'align-regexp)
(bind-key "M-SPC d g s" #'magit-status)
(bind-key "M-SPC d g b" #'magit-blame)
(bind-key "M-SPC d g d" #'magit-diff)
(bind-key "M-SPC d v c a" #'vc-annotate)
(bind-key "M-SPC d v c d" #'vc-diff)
(bind-key "M-SPC d w o" #'just-one-space)
(bind-key "M-SPC d w d" #'delete-horizontal-space)
(bind-key "M-SPC d i" #'indent-rigidly)
(bind-key "M-SPC d c" #'comment-or-uncomment-region)
(bind-key "M-SPC d d" #'~duplicate-line-or-region)
(bind-key "M-SPC d k" #'kill-sexp)
(bind-key "M-SPC d z" #'repeat)

;; External exec
(bind-key "M-SPC a e" #'~palette/exec-sh-in-term-mux-then-pause)
(bind-key "M-SPC a i" #'~palette/point/exec-sh-in-term-mux)
(bind-key "M-SPC a k" #'~palette/point/exec-sh-piping-here)
(bind-key "M-SPC a x" #'~palette/point/exec-sh-in-term-mux-then-pause)
(bind-key "M-SPC a a" #'~ansi-colorize-current-output-block)
(bind-key "M-SPC a s" #'(lambda ()
                          (interactive)
                          (~palette/exec-sh-in-term-mux "zsh")))

;; Text exec
(bind-key "M-SPC x |" #'~exec-sh|)
(bind-key "M-SPC x <" #'~exec-sh<)
(bind-key "M-SPC x >" #'~exec-sh>)
(bind-key "M-SPC x p" #'~exec-sh-pop-up)
(bind-key "M-SPC x x" #'~execute-text-prompt)
(bind-key "M-SPC x l" #'~execute-line)
(bind-key "M-SPC x w" #'~execute-current-wand-text)
(bind-key "M-SPC x b" #'~exec-current-block)
(bind-key "M-SPC x L" #'(lambda ()
                          (interactive)
                          (call-interactively #'~execute-line)
                          (call-interactively #'kill-current-buffer)))
(bind-key "M-SPC x e" #'~execute)
(bind-key "M-SPC x E" #'(lambda ()
                          (interactive)
                          (call-interactively #'~execute)
                          (call-interactively #'kill-current-buffer)))
(bind-key "M-SPC x j" (~make-repeatable-fn #'~goto-prev-command-pattern))
(bind-key "M-SPC x k" (~make-repeatable-fn #'~goto-next-command-pattern))
(bind-key "M-SPC x r" #'~insert-entry-from-exec-history)

;; Insertion
(bind-key "M-SPC i ;" #'~insert-full-line-comment)
(bind-key "M-SPC i <" #'(lambda () (interactive) (insert *~output-beginning-marker*)))
(bind-key "M-SPC i >" #'(lambda () (interactive) (insert *~output-end-marker*)))
(bind-key "M-SPC i b" #'(lambda () (interactive) (insert *~output-beginning-marker* "\n" *~output-end-marker*) (previous-line)))
(bind-key "M-SPC i x" #'~insert-entry-from-exec-history)
(bind-key "M-SPC i e" #'~insert-exec)

;; Frame
(bind-key "M-SPC r n" #'make-frame)
(bind-key "M-SPC r k" #'delete-frame)

;; Window
(bind-key "M-SPC w s r" (~make-repeatable-fn #'~split-window-right))
(bind-key "M-SPC w s b" (~make-repeatable-fn #'~split-window-below))
(bind-key "M-SPC w k" (~make-repeatable-fn #'delete-window))
(bind-key "M-SPC w T" (~make-repeatable-fn #'~scroll-other-window))
(bind-key "M-SPC w C" (~make-repeatable-fn #'~scroll-other-window-reverse))
(bind-key "M-SPC w o" #'~one-window)
(bind-key "M-SPC w t" (~make-repeatable-fn #'~transpose-windows))
(bind-key "M-SPC w y" (~make-repeatable-fn #'~toggle-sticky-window))
(bind-key "M-SPC w z" (~make-repeatable-fn #'~toggle-maximize-buffer))
(with-eval-after-load "ace-window"
  (bind-key "M-SPC w p" #'ace-swap-window)
  (bind-key "M-SPC w p" #'ace-window))
(bind-key "M-SPC w r" #'resize-window)
(bind-key "M-SPC w w" (~make-repeatable-fn #'other-window))

;; Parens management
(with-eval-after-load "smartparens"
  (bind-key "M-SPC p )" (~make-repeatable-fn #'sp-forward-slurp-sexp))
  (bind-key "M-SPC p (" (~make-repeatable-fn #'sp-backward-slurp-sexp))
  (bind-key "M-SPC p }" (~make-repeatable-fn #'sp-forward-barf-sexp))
  (bind-key "M-SPC p {" (~make-repeatable-fn #'sp-backward-barf-sexp))
  (bind-key "M-SPC p l" (~make-repeatable-fn #'sp-forward-sexp))
  (bind-key "M-SPC p h" (~make-repeatable-fn #'sp-backward-sexp))
  (bind-key "M-SPC p j" (~make-repeatable-fn #'sp-down-sexp))
  (bind-key "M-SPC p k" (~make-repeatable-fn #'sp-backward-up-sexp))
  (bind-key "M-SPC p e s" (~make-repeatable-fn #'sp-split-sexp))
  (bind-key "M-SPC p e S" (~make-repeatable-fn #'sp-splice-sexp))
  (bind-key "M-SPC p e j" (~make-repeatable-fn #'sp-join-sexps)))

;; Mode
(bind-key "M-SPC n SPC" (~make-repeatable-fn #'whitespace-mode))
(bind-key "M-SPC n g f" (~make-repeatable-fn #'global-font-lock-mode))
(bind-key "M-SPC n l f" (~make-repeatable-fn #'font-lock-mode))
(bind-key "M-SPC n w" (~make-repeatable-fn #'~toggle-soft-wrapping))

;; TODO
;; Context menu
;; ~execute
;; ~read-command-or-get-from-secondary-selection
;; things-cmd: thing-at-point 'exec-text
;; Key: ~toggle-maximize-buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished bootstrapping functionality")

(provide 'rmacs:bootstrap-functionality)
