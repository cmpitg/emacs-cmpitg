;;  -*- lexical-binding: t; -*-

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
;; But do customize current font
(set-frame-font "Cascadia Code-11" nil t)
(add-to-list 'default-frame-alist
             '(font . "Cascadia Code-11"))
;; (set-frame-font "Fira Code Medium-10.5" nil t)
;; (add-to-list 'default-frame-alist
;;              '(font . "Fira Code Medium-10"))
;; (set-frame-font "Go Mono-10.5" nil t)
;; (add-to-list 'default-frame-alist
;;              '(font . "Go Mono-10.5"))

;; Set line spacing
(setq-default line-spacing 2)

;; Restore cursor position after scrolling
;; Ref: http://elpa.gnu.org/packages/scroll-restore.html
;; FIXME: Buggy - Check and fix
(use-package scroll-restore
  :disabled t
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

;; Smooth scrolling

;; Ref: https://github.com/zk-phi/sublimity
;; Sublimity makes CPU scream
;; (require 'sublimity-scroll)
;; (setq sublimity-scroll-weight 10
;;       sublimity-scroll-drift-length 6)
;; (sublimity-mode 1)

;; The smooth-scrolling makes cursor jump unexpectedly
;; (require 'smooth-scrolling)
;; (smooth-scrolling-mode 1)

;; Ref: https://github.com/emacs-mirror/emacs/blob/master/lisp/pixel-scroll.el
;; Ref: https://emacs.stackexchange.com/questions/10354/smooth-mouse-scroll-for-inline-images
;; (require 'pixel-scroll)
;; (pixel-scroll-mode -1)
;; (pixel-scroll-mode 1)
;; (setq pixel-dead-time 0)               ; Never go back to the old scrolling behaviour
;; (setq pixel-resolution-fine-flag t)    ; Scroll by number of pixels instead of lines (t = frame-char-height pixels)
;; (setq mouse-wheel-scroll-amount '(1))  ; Distance in pixel-resolution to scroll each mouse wheel event

;; Save and restore current editing point when opening a file
(use-package saveplace
  :config (save-place-mode 1)
  :init (progn
          (custom-set-variables `(save-place-file ,(format "~/.emacs.d/places.%s" server-name)))))

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

;; Added functionality to hippie-expand
;; (add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;; Hide undo-tree from mode line
(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

;; Make shebang-ed files executable
(add-hook 'after-save-hook #'~maybe-make-current-file-executable)

;; Clean up all Tramp remote connection before killing Emacs
(add-hook 'kill-emacs-hook #'~clean-up-tramp)

;; Delete file/window/frame when killing buffer if necessary
(defun ~advice/delete-linked-windows (orig-fun &rest args)
  "Deletes linked windows."
  (apply orig-fun args)
  (~delete-linked-windows))
(add-hook 'kill-buffer-hook #'~maybe-delete-file-when-killing-buffer)
(add-hook 'kill-buffer-hook #'~maybe-delete-window-when-killing-buffer)
(add-hook 'kill-buffer-hook #'~maybe-delete-frame-when-killing-buffer)
(advice-add #'delete-window :around #'~advice/delete-linked-windows)

;; Track recently closed files
(add-hook 'kill-buffer-hook #'~track-closed-file)

;; Track frame focus
(cl-defun ~save-frame-focus-time (&optional (frame (selected-frame)))
  "Saves the focus time for a frame."
  (set-frame-parameter frame :custom/focus-time (time-to-seconds (current-time))))
(add-function :after after-focus-change-function #'~save-frame-focus-time)

;; FIXME
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

;; Buffers are popped up in a separate frame or window
(~set-pop-up-buffer-mode :window)

;; Make window combinations resize proportionally
(setq window-combination-resize t)

;; One buffer per window setup
;; (require 'rmacs:config-one-buffer-per-window "config-one-buffer-per-window")
;; We don't want to have the behavior but still want to keep the blank buffer
;; TODO: Make the following call into an enable/disable call
;; (setq *one-buffer-per-window-temporarily-disabled?* t)

;; Acme-like command palette utils.
(require 'rmacs:config-module-command-palette "config-module-command-palette")
(command-palette-mode -1)

;; Ojo - Inserting eval result back to current buffer
(require 'rmacs:config-module-ojo "config-module-ojo")
(ojo:enable)

;; Most-recently-used (MRU) buffer behavior
;; Ref: https://github.com/jrosdahl/iflipb
(use-package iflipb
  ;; :straight
  ;; (iflipb :type git :host github :repo "jrosdahl/iflipb")
  :config (custom-set-variables `(iflipb-ignore-buffers nil)))

;; Simple buffer listing
(require 'rmacs:config-module-simple-buffer-list "config-module-simple-buffer-list")

;; Better display of header-line
(set-face-attribute 'header-line nil
                    :family "Roboto"
                    :height 130
                    :width 'condensed
                    :weight 'bold)

;; Mode line format
(set-face-attribute 'mode-line nil
                    :family "Go"
                    :height 130
                    :width 'condensed)
(setq ~mode-line-simplified-position
      `(line-number-mode
        ((column-number-mode
          (column-number-indicator-zero-based (10 #(" (%l,%c)" 0 8 nil))
                                              (10 #(" (%l,%C)" 0 8 nil)))
          (6 #(" L%l" 0 4 nil))))
        ((column-number-mode
          (column-number-indicator-zero-based (5 #(" C%c" 0 4 nil))
                                              (5 #(" C%C" 0 4 nil)))))))
(setq ~mode-line-format
      `("%e"               ;; Displays out-of-mem error if there is
        mode-line-client   ;; Identifies an Emacsclient frame
        mode-line-modified ;; Identifies if the current buffer is modified
        mode-line-auto-compile
        mode-line-remote               ;; Indicates a remote buffer
        mode-line-frame-identification ;; Describes current frame
        ~mode-line-simplified-position ;; Show column & line numbers
        mode-line-buffer-identification
        ;; (:eval evil-mode-line-tag)
        mode-line-modes
        mode-line-misc-info))
(setq-default mode-line-format ~mode-line-format)
(setq mode-line-format ~mode-line-format)
(defun ~format-mode-line-for-frame (frame)
  (interactive)
  (with-selected-frame frame
    (setq mode-line-format ~mode-line-format)))
(add-to-list 'after-make-frame-functions #'~format-mode-line-for-frame)

;; Displaying available keybindings in pop up
;; Ref: https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :disabled t
  :config (progn
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)))

;; Highlighting phrase and expression when needed
;; Ref: https://www.emacswiki.org/emacs/HiLock
(use-package hi-lock
  :commands (highlight-phrase highlight-regexp))

;; Soft-wrapping long lines
;; Ref: https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :demand t
  :init (progn
          (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

          (defun ~turn-on-soft-wrapping ()
            "Turns on soft-wrapping."
            (interactive)
            (turn-off-auto-fill)
            (turn-on-visual-line-mode)
            (turn-on-visual-fill-column-mode))

          (defun ~turn-off-soft-wrapping ()
            "Turns off soft-wrapping."
            (interactive)
            (visual-line-mode -1)
            (visual-fill-column-mode -1))

          (defun ~toggle-soft-wrapping ()
            "Toggles on soft-wrapping mode."
            (interactive)
            (if visual-fill-column-mode
                (~turn-off-soft-wrapping)
              (~turn-on-soft-wrapping)))

          ;; Correct the default split
          (setf split-window-preferred-function
                #'visual-fill-column-split-window-sensibly)

          ;; Show visual indicators for logical lines
          (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

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

;; Displaying eval result in an overlay after eval'ing
;; Ref: http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
;; TODO - URGENT Clean up the following code

(use-package cider)
(autoload 'cider--make-result-overlay "cider-overlays")

(defun blink:display-value (value point)
  "Displays a value in an overlay at a point."
  (let ((comment-start (if (null comment-start) ";" comment-start)))
    (cider--make-result-overlay (format "%S" value)
      :where point
      :duration 'command))
  value)

(defun blink:display-value-at-point (value)
  "Displays `value' in an overlay at current point."
  (blink:display-value value (point)))

(defun blink:display-value-at-end-of-selection (value)
  "Displays `value' in an overlay at the end of the current
selection or current point."
  (blink:display-value value (if (region-active-p)
                                 (region-end)
                               (point))))

(defun blink:display-value-at-end-of-defun (value)
  "Displays `value' in an overlay at end-of-defun."
  (blink:display-value value (save-excursion
                               (end-of-defun)
                               (point))))

(defun blink:display-value-at-end-of-selection-or-defun (value)
  "Displays `value' in an overlay at the end of the current
selection or end-of-defun."
  (blink:display-value value (if (region-active-p)
                                 (region-end)
                               (save-excursion
                                 (end-of-defun)
                                 (point)))))

(defun blink:display-value-at-end-of-selection-or-line (value)
  "Displays `value' in an overlay at the end of the current
selection or end-of-line."
  (blink:display-value value (if (region-active-p)
                                 (region-end)
                               (point-at-eol))))

(defun blink:display-value-at-end-of-sexp (value)
  "Displays `value' in an overlay at end of the current sexp."
  (blink:display-value-at-end-of-defun value (save-excursion
                                               (sp-up-sexp)
                                               (point))))

(defun blink:enable ()
  (interactive)
  (advice-add 'eval-last-sexp :filter-return #'blink:display-value-at-point)
  (advice-add 'pp-eval-last-sexp :filter-return #'blink:display-value-at-point)
  (advice-add 'eval-defun :filter-return #'blink:display-value-at-end-of-defun)
  (advice-add '~eval-current-sexp :filter-return #'blink:display-value-at-end-of-sexp)
  (advice-add 'eval-region :filter-return #'blink:display-value-at-end-of-selection)
  (advice-add '~eval-region :filter-return #'blink:display-value-at-end-of-selection)
  (advice-add '~execute :filter-return #'blink:display-value-at-end-of-selection-or-line)
  (advice-add '~execute-line :filter-return #'blink:display-value-at-end-of-selection-or-line))

(defun blink:disable ()
  (interactive)
  (advice-remove 'eval-last-sexp #'blink:display-value-at-point)
  (advice-remove 'pp-eval-last-sexp #'blink:display-value-at-point)
  (advice-remove 'eval-defun #'blink:display-value-at-end-of-defun)
  (advice-remove '~eval-current-sexp #'blink:display-value-at-end-of-sexp)
  (advice-remove 'eval-region #'blink:display-value-at-end-of-selection)
  (advice-remove '~eval-region #'blink:display-value-at-end-of-selection)
  (advice-remove '~execute #'blink:display-value-at-end-of-selection-or-line)
  (advice-remove '~execute-line #'blink:display-value-at-end-of-selection-or-line))

(blink:enable)
;; (blink:disable)

;; ;; Buffer list sidebar
;; ;; Ref: https://github.com/jojojames/ibuffer-sidebar
;; (use-package ibuffer-sidebar
;;   :bind (("<C-f8>" . ibuffer-sidebar-toggle-sidebar)
;;          :map ibuffer-name-map
;;          ("<double-mouse-1>" . ibuffer-visit-buffer))
;;   :commands (ibuffer-sidebar-toggle-sidebar)
;;   :config
;;   (progn
;;     (setq ibuffer-sidebar-use-custom-font t)
;;     (setq ibuffer-sidebar-display-alist '((side . right) (slot . 1)))))

;; ;; Convenient display of buffer list, helpful when using mouse
;; (setq ibuffer-formats '((mark modified read-only locked
;;                               " "
;;                               (name 50 -1 :left :elide)
;;                               " "
;;                               filename-and-process)))

;; Do not close windows when doing keyboard-quit
(defun ~advice/do-not-close-windows (fun &rest args)
  (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
    (apply fun args)))

(advice-add #'keyboard-escape-quit :around #'~advice/do-not-close-windows)

;; Emoji
;; Ref: https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :ensure t
  :init (global-emojify-mode))

;; Visible white spaces
(use-package whitespace
  :init
  (progn
    (global-whitespace-mode 1)
    ;; (setq whitespace-style '(tab-mark newline-mark))
    ;; (setq whitespace-style '(tab-mark))
    ;; (setq whitespace-style '(face tabs spaces trailing lines newline empty tab-mark))
    ;; (setq whitespace-style '(face tabs spaces trailing newline empty tab-mark))
    (setq whitespace-style '(face tabs trailing newline empty tab-mark))
    ;; (setq whitespace-display-mappings
    ;;       '((newline-mark ?\n [?¬ ?\n] [?$ ?\n])
    ;;         (space-mark ?\ [?·] [?.])
    ;;         (space-mark ?\xA0 [?¤] [?_])
    ;;         (tab-mark ?\t [?» ?\t] [?\\ ?\t])))
    (setq whitespace-display-mappings
          '((newline-mark ?\n [?¬ ?\n] [?$ ?\n])
            (space-mark ?\xA0 [?¤] [?_])
            (tab-mark ?\t [?» ?\t] [?\\ ?\t])))))
(setq-default show-trailing-whitespace t)

;; Resize window
;; Ref: https://github.com/dpsutton/resize-window
(use-package resize-window
  :ensure t
  :config
  (progn
    (setq resize-window-fine-argument 3)

    (dolist (action (list (list ?n #'resize-window--enlarge-horizontally " Resize - horizontally" t)
                          (list ?h #'resize-window--shrink-horizontally " Resize - shrink horizontally" t)
                          (list ?t #'resize-window--enlarge-down " Resize - Expand down" t)
                          (list ?c #'resize-window--enlarge-up " Resize - Expand up" t)))
      (add-to-list 'resize-window-dispatch-alist action))))

;; Window jumping
;; Ref: https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :config
  (progn
    (custom-set-variables `(aw-keys (list ?u ?e ?o ?a ?i ?h ?t ?n ?s ?k ?j ?q ?' ?x ?m ?w ?v ?z ?b)))))

;; Acme-mouse
;; Ref: https://github.com/cmpitg/acme-mouse
(use-package acme-mouse
  :disabled t
  :config
  (progn
    (acme-mouse-mode)
    (global-acme-mouse-mode)))

;; Allow text drap-and-drop with mouse
(custom-set-variables `(mouse-drag-and-drop-region t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distraction-free writing
;; Ref: https://github.com/rnkn/olivetti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package olivetti
  :init
  (progn
    ;; (add-hook 'org-mode-hook #'turn-on-olivetti-mode)
    (setq-default olivetti-body-width 92)))

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

(message "Finished configuring core UX")

(provide 'rmacs:config-core-ux)
