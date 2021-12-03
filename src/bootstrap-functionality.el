;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2021 Ha-Duong Nguyen (@cmpitg)
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
  (require 'cl-lib))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defalias '~is-selecting? 'use-region-p
    "Determines if current there is a selection/active region.")

  (defun ~get-selection ()
    "Gets the currently selected text."
    (if (~is-selecting?)
        (buffer-substring (region-beginning) (region-end))
      ""))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ~current-dir ()
    "Current directory."
    (or (file-name-directory (or load-file-name buffer-file-name ""))
        ""))

  (defun ~current-file-full-path ()
    "Full path to current file."
    (or (expand-file-name (or buffer-file-name ""))
        ""))

  (defun ~clean-up-tramp ()
    "Closes all tramp connections and buffers."
    (interactive)
    (tramp-cleanup-all-connections)
    (tramp-cleanup-all-buffers))

  (defun ~get-last-sexp ()
    "Returns the last sexp before the current point."
    (interactive)
    (elisp--preceding-sexp))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Window & Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; File & buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tracking recently closed files
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Process/Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                            command))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "<menu>" 'nil)
(bind-key "s-SPC" 'nil)

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

;; Emacs Lisp
(bind-key "<C-return>"   #'~eval-last-sexp-or-region)
(bind-key "<M-return>"   #'eval-defun)

;; Function keys & other convenient bindings
(bind-key "<f2>" #'save-buffer)
(bind-key "<f3>" #'find-file)
(bind-key "<C-f4>" #'kill-current-buffer)
;; BUG: Command history not recorded
;; (bind-key "<f12>" #'~ido-M-x)
(bind-key "<f12>" #'execute-extended-command)

;; TODO
;; Context menu
;; ~execute
;; ~read-command-or-get-from-secondary-selection
;; things-cmd: thing-at-point 'exec-text
;; Key: ~toggle-maximize-buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished bootstrapping functionality")

(provide 'rmacs:bootstrap-functionality)