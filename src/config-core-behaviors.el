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

;; 3 lines at a time normally, 5 lines at a time with shift
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll 3 lines at a time
(setq scroll-step 3)
(setq scroll-conservatively 10000)

;; Restore cursor position after scrolling
;; Ref: http://elpa.gnu.org/packages/scroll-restore.html
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

            ;; Due to some reason the mode needs disabling and re-enabling to
            ;; work
            (scroll-restore-mode -1)
            (scroll-restore-mode 1)))

;; Disable Tramp autosave
(setq tramp-auto-save-directory "/tmp/")

;; Don't let the cursor go into minibuffer prompt
;; Ref: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Use the same clipboard with X
(setq x-select-enable-clipboard t)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;; Always suggest keybindings
(setq suggest-key-bindings t)

;; More tolerable stack
(setq max-lisp-eval-depth 15000)
(setq max-specpdl-size    15000)

;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Maximum number of ring items to store
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

;; Line numbering is off by default
(use-package linum
  :init (progn
          (global-linum-mode -1)
          (setq linum-format "%d ")))

;; Don't highlight the current line
(hl-line-mode -1)

;; Added functionality to hippie-expand
(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Hide undo-tree from mode line
(use-package undo-tree
  :diminish undo-tree-mode)

;; Make shebang-ed files executable
(add-hook 'after-save-hook #'~maybe-make-current-file-executable)

;; Clean up all Tramp remote connection before killing Emacs
(add-hook 'kill-emacs-hook #'~clean-up-tramp)

;; Delete file when killing buffer if necessary
(add-hook 'kill-buffer-hook #'~maybe-delete-file-when-killing-buffer)

;; Track recently closed files
(add-hook 'kill-buffer-hook #'~track-closed-file)

;; Focus follows mouse
(setq mouse-autoselect-window t)

;; Set frame title
(let ((title-format
       `("Rmacs"
         ,(format " @ %s" (or *emacs-as-tool*
                              :edit))
         " \u262f "
         (buffer-file-name "%f"
                           (dired-directory dired-directory "%b")))))
  (setq-default frame-title-format title-format)
  (setq frame-title-format title-format))

;; Soft-wrapping long lines
;; Ref: https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :init (progn
          (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

          ;; Correct the default split
          (setf split-window-preferred-function
                #'visual-fill-column-split-window-sensibly)

          (with-eval-after-load "evil"
            ;; Make movement keys work like they should
            (define-key evil-normal-state-map
              (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
            (define-key evil-normal-state-map
              (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
            (define-key evil-motion-state-map
              (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
            (define-key evil-motion-state-map
              (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
            ;; Make horizontal movement cross lines
            (setq-default evil-cross-lines t))))

;; Preventing other windows from stealing the current window
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))
(ad-activate 'pop-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core behaviors")

(provide 'rmacs:config-core-behaviors)
