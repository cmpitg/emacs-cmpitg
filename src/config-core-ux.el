;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018-2022 Ha-Duong Nguyen (@cmpitg)
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

;; Hide the toolbar
(tool-bar-mode -1)

;; Added functionality to hippie-expand
;; (add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;; Hide undo-tree from mode line
(use-package undo-tree
  :diminish undo-tree-mode
  :custom (undo-tree-auto-save-history . nil)
  :config
  (progn
    (global-undo-tree-mode)))

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
  :config (progn
            (which-key-mode 1)
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

;; When doing interactive search, try taking current region first
(defun ~advice/isearch-taking-current-region (fun forward &rest args)
  (let ((current-selection (buffer-substring-no-properties (mark) (point))))
    (cond ((and transient-mark-mode mark-active (not (eq (mark) (point))))
           (isearch-update-ring current-selection)
           (deactivate-mark)
           (apply fun forward args)
           (if (not forward)
               (isearch-repeat-backward)
             (goto-char (mark))
             (isearch-repeat-forward)))
          (t
           (cond ((string-empty-p current-selection)
                  (apply fun forward args))
                 (t
                  (when mark-active
                    (isearch-update-ring current-selection))
                  (deactivate-mark)
                  (apply fun forward args)
                  (cond ((not forward)
                         (isearch-repeat-backward))
                        (t
                         (goto-char (mark))
                         (isearch-repeat-forward)
                         (isearch-repeat-forward)))))))))

(advice-add #'isearch-mode :around #'~advice/isearch-taking-current-region)

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
