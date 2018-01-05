;; -*- no-byte-compile: t -*-

;;
;; Copyright (C) 2014-2017 Ha-Duong Nguyen (@cmpitg)
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

(defvar *electrify-return-match*
  "[\]\)]"
  ;; "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an
\"electric\" return.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3 lines at a time normally, 5 lines at a time with shift
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll 3 lines at a time
(setq scroll-step 3)
(setq scroll-conservatively 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restore cursor position after scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://elpa.gnu.org/packages/scroll-restore.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scroll-restore
  :config (progn
            ;; Recenter when jumpting back
            (setq scroll-restore-center t)

            ;; Allow scroll-restore to modify the cursor face
            (setq scroll-restore-handle-cursor t)

            ;; Make the cursor invisible while POINT is off-screen
            (setq scroll-restore-cursor-type nil)

            ;; Jump back to the original cursor position after scrolling
            (setq scroll-restore-jump-back t)

            (scroll-restore-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable Tramp autosave
(setq tramp-auto-save-directory "/tmp/")

;; Default scratch-buffer mode
;; (setq-default initial-major-mode 'emacs-lisp-mode)
(setq-default initial-major-mode 'adoc-mode)
(setq-default major-mode 'adoc-mode)

;; Don't let the cursor go into minibuffer prompt
;;   http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;; Use the same clipboard with X
(setq x-select-enable-clipboard t)

;; Disable shift selection
(setq shift-select-mode nil)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;; Always suggest keybindings
(setq suggest-key-bindings t)

;; More tolerable stack
(setq max-lisp-eval-depth 15000
      max-specpdl-size    15000)

;; Never change case when replacing
(setq-default case-replace nil)

;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Set mark-ring-max
(setq mark-ring-max 512)

;; yes/no questions become y/n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backspace and Del delete selection, except in paredit-mode
(delete-selection-mode 1)

;; Set the default tab width
(setq default-tab-width 4)

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

;; 1 use mouse, so scroll bar comes in handy
;; (scroll-bar-mode -1)
(scroll-bar-mode 1)
(set-scroll-bar-mode 'left)

;; (menu-bar-mode -1)
(menu-bar-mode 1)

;; Display time
;; (display-time)
(display-time-mode -1)

;; Turn off welcome message
(setq inhibit-startup-message t)

;; Display the size of the buffer
(size-indication-mode 1)

;; Show the battery indicator
(display-battery-mode -1)

;; Vertical split
;; (setq split-width-threshold nil)
;; Horizontal split
;; (setq split-width-threshold 1)

;;; Blink cursor
;; (blink-cursor-mode t)
(blink-cursor-mode -1)

;; Change cursor type
;; (set-default 'cursor-type 'hbar)
;; (set-default 'cursor-type 'box)
(set-default 'cursor-type 'bar)

;; Show matching parentheses
(show-paren-mode 1)
;; (show-paren-mode -1)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'expression)

;; Set ispell-dictionary
;; (ispell-change-dictionary "american")

;; grep command
(setq grep-command "grep -i -nH -e ")

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

;; minibuffer window expands vertically as necessary to hold the text
;; that you put in the minibuffer
(setq resize-mini-windows t)

;; Never change case when replacing
(setq-default case-replace nil)

;; Set the appropriate frame size
;; (set-frame-size-according-to-resolution)

(use-package linum
  :init (progn
          (global-linum-mode -1)
          (setq linum-format "%d ")))

;; Don't use font lock by default
(global-font-lock-mode -1)
(setq font-lock-maximum-size nil)
(add-hook 'adoc-mode-hook
          (lambda ()
            (font-lock-mode -1)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (font-lock-mode -1)))

;; Don't highlight the current line
(hl-line-mode -1)

;; Added functionality to hippie-expand
(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Make shebang-ed files executable
(add-hook 'after-save-hook '~make-executable)

;; Clean up all Tramp remote connection before killing Emacs
(add-hook 'kill-emacs-hook '~clean-up-tramp)

;; Diminish auto-revert-mode in the mode line
(diminish 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Right click context menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~right-click-menu ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq frame-title-format
;;       '(multiple-frames "%b" ("@" system-name )))
;; (let ((emacs-as (--> (or (~emacs-as) :emacs)
;;                      (symbol-name it)
;;                      (substring it 1)
;;                      (intern it)
;;                      (symbol-name it)
;;                      (capitalize it))))
;;   (setq-default frame-title-format `(,emacs-as ,(format " @ %s" (or *emacs-as-tool*
;;                                                                     :edit))
;;                                                " \u262f "
;;                                                (buffer-file-name "%f"
;;                                                                  (dired-directory dired-directory "%b")))))

(let ((title-format `("Rmacs"
                      ,(format " @ %s" (or *emacs-as-tool*
                                           :edit))
                      " \u262f "
                      (buffer-file-name "%f"
                                        (dired-directory dired-directory "%b")))))
  (setq-default frame-title-format title-format)
  (setq frame-title-format title-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq css-indent-offset 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus following mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq mouse-autoselect-window t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soft-wrap long lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In text-related modes only, don't mess up with code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; I have tried adaptive-wrap but didn't like it.  It gave false impression of
;; how a line is actually wrapped.  The code doesn't handle different cases as
;; nicely as visual-fill-column
;;

;; https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :init (progn
          (defun ~turn-on-soft-wrapping ()
            "Turns on soft-wrapping."
            (interactive)
            (turn-off-auto-fill)
            (turn-on-visual-line-mode)
            (turn-on-visual-fill-column-mode))

          (defun ~turn-off-soft-wrapping ()
            (interactive)
            (visual-line-mode -1)
            (visual-fill-column-mode -1))

          (defun ~toggle-soft-wrapping ()
            "Toggles on soft-wrapping mode."
            (interactive)
            (if visual-fill-column-mode
                (~turn-off-soft-wrapping)
              (~turn-on-soft-wrapping)))

          (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
          ;; Correct the default split
          (setf split-window-preferred-function #'visual-fill-column-split-window-sensibly)

          ;; Make movement keys work like they should
          (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
          (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
          (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
          (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
          ;; Make horizontal movement cross lines
          (setq-default evil-cross-lines t)

          (add-hook 'markdown-mode-hook #'~turn-on-soft-wrapping)
          (add-hook 'adoc-mode-hook #'~turn-on-soft-wrapping)))

;; (ignore-errors
;;   (diminish 'visual-line-mode)
;;   (diminish 'global-visual-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete the corresponding file when killing buffer if needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~delete-file-when-killing-buffer ()
  "Deletes current file when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-on-exit)
             local/delete-on-exit)
    (~delete-current-file)))

(add-hook 'kill-buffer-hook #'~delete-file-when-killing-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Down mouse 1 should change evil to insert mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~mouse-1-evil-insert-mode-advice-around (orig-fun &rest args)
  (interactive)
  (let ((res (call-interactively orig-fun))
        (old-point (point)))
    (call-interactively 'evil-insert)
    ;; After calling evil-insert, the cursor moves the beginning of the region
    ;; so we need to set it back
    (when (< (point) old-point)
      (call-interactively 'exchange-point-and-mark))
    res))

(advice-add 'evil-mouse-drag-region
            :around #'~mouse-1-evil-insert-mode-advice-around)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displaying available keybindings in pop up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/justbur/emacs-which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :diminish which-key-mode
  :config (progn
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preventing other windows from stealing the current window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))
(ad-activate 'pop-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading essential UX customizations")
(provide 'ee:config-core-ux)
