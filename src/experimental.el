;;
;; Copyright (C) 2014-2020 Ha-Duong Nguyen (@cmpitg)
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

(setf *open-with-regexps*
      `(("\\.pdf" . "evince '%s'")))

;;
;; Aliases
;;

(defalias '+col #'split-window-horizontally)
(defalias '+row #'split-window-vertically)
(defalias '+buf #'~new-buffer)
(defalias '+frame #'make-frame)
(defalias '+-tool #'~toggle-project-toolbox)
(defalias '-buf #'kill-current-buffer)
(defalias '-window #'~delete-window)
(defalias '-me #'~kill-buffer-and-window)
(defalias '-frame #'~kill-buffer-and-frame)

(defalias 'save-cp #'command-palette:save-cp)
(defalias 'ojo #'ojo-mode)

;;
;; Auto-compile Emacs Lisp unless already compiled
;;
;; Ref: https://github.com/emacscollective/auto-compile
;;

(setq load-prefer-newer t)
(use-package auto-compile
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;; (setenv "XDG_DATA_DIRS" "/usr/share/i3:/usr/local/share:/usr/share")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distraction-free writing
;; Ref: https://github.com/rnkn/olivetti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package olivetti
  :ensure t
  :config
  (progn
    (add-hook 'org-mode-hook #'turn-on-olivetti-mode)
    (setq-default olivetti-body-width 92)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs, don't move my window
;; Ref: https://www.reddit.com/r/emacs/comments/llvyxe/hey_emacs_dont_move_my_windows_customizing/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-buffer-alist
      '((".*"
         (display-buffer-reuse-window display-buffer-same-window)
         ;; (reusable-frames . t)
         )))

;; display-buffer: avoid resizing
(setq even-window-sizes nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~choose-path-and-act ()
  "Interactively chooses a path and does something with it."
  (interactive)
  (lexical-let ((path (read-file-name "Path: " nil nil 'confirm)))
    (unless (string-empty-p path)
      (defhydra local-hydra-act-on-path (:columns 4 :exit t)
        "What to do?"
        ("c" #'(lambda () (interactive) (~copy-to-clipboard path)) "Copy")
        ("o" #'(lambda () (interactive) (find-file path)) "Open")
        ("i" #'(lambda () (interactive) (insert path)) "Insert here"))
      ;; TODO: Open with external program

      (local-hydra-act-on-path/body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posframe: Child frame utils
;; Ref: https://github.com/tumashu/posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package posframe
  :ensure t)

;; TODO: Review all the custom-set-variables -> customize-set-variable
;; TODO: https://github.com/abo-abo/hydra/blob/master/hydra.el - read docs thoroughly
;; TODO: Use defhydra+
;; TODO: Check out https://github.com/honmaple/emacs-maple-minibuffer
;; TODO: Check out https://github.com/tumashu/ivy-posframe
;; TODO: Ref https://github.com/tumashu/posframe
;; TODO: Check out https://www.reddit.com/r/emacs/comments/cdfr20/ann_majormodehydra_prettyhydra_020/
;; TODO: Emacs Lucid's set-frame-position is too slow: https://github.com/tumashu/posframe/issues/45
;; TODO: Check out https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-27&id=c49d379f17bcb0ce82604def2eaa04bda00bd5ec
;; TODO: Check out https://github.com/tumashu/company-posframe/issues/2
;; TODO: Check out https://github.com/abo-abo/hydra/wiki/Hydras-by-Topic

(customize-set-variable 'hydra-hint-display-type 'posframe)
(customize-set-variable 'hydra-hint-display-type 'lv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using Hydra in Posframe
;; Ref: https://github.com/Ladicle/hydra-posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra-posframe
  :straight
  (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
  :after (posframe)
  :config
  (progn
    (hydra-posframe-mode)
    (customize-set-variable 'hydra-posframe-border-width 3)
    (customize-set-variable 'hydra-posframe-border-face 'black)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command palette utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ~with-output-to-next-window (goto-beginning? &rest body)
  "TODO"
  (when (= 1 (~count-windows))
    (error "Must have must than one window to output"))

  `(with-current-buffer (window-buffer (next-window))
     (when ,goto-beginning?
       (beginning-of-buffer))
     (open-line 2)
     ,@body))
(put '~with-output-to-next-window 'lisp-indent-function 1)

(defmacro ~with-output-to-buffer-file (path goto-beginning? &rest body)
  "TODO"
  `(with-current-buffer (~visit-file ,path)
     (when ,goto-beginning?
       (beginning-of-buffer))
     (open-line 2)
     ,@body))
(put '~with-output-to-buffer-file 'lisp-indent-function 2)

(defun ~palette/ensure-prefix (text prefix)
  "Makes sure text has a prefix."
  (lexical-let* ((text (concat prefix (~palette/trim-garbage text)))
                 (padding (first (s-match (rx bos (0+ blank)) text))))
    (concat padding text)))

;; (~palette/ensure-prefix "         ls \\\n" "$ ")
;; (~palette/ensure-prefix "ls \\\n" "$ ")
;; (~palette/ensure-prefix "ls \\" "$ ")
;; (~palette/ensure-prefix "   $!! hello world !!$ aa" "$ ")
;; (~palette/ensure-prefix "   hello !!! world" "$ ")
;; (~palette/ensure-prefix "         pwd" "$ ")
;; (~palette/ensure-prefix "         ls\\\n" "$ ")

(defun ~palette/trim-garbage (text)
  "TODO. TODO: Customize garbage"
  (lexical-let* ((elements (s-split-up-to (rx bos (1+ (any " ;!$#"))) text 1))
                 (text (if (> (length elements) 1)
                           (string-trim (second elements))
                         (first elements)))
                 (pass-2 (s-split-up-to (rx bos (>= 2 "/")) text 1))
                 (res (if (> (length pass-2) 1)
                          (string-trim (second pass-2))
                        (first pass-2))))
    res))

;; (~palette/trim-garbage "ls \\\n")
;; (~palette/trim-garbage "   ls \\\n")
;; (~palette/trim-garbage "!!$   ls \\\n")
;; (~palette/trim-garbage "   ls $  sss $\\\n")
;; (~palette/trim-garbage "ls \\\n -1 ~/tmp/")
;; (~palette/trim-garbage "/usr/bin/ls \\\n -1 ~/tmp/")

(defun ~palette/decorate-exec-text-at-point ()
  "Decorates executable text at current point.  Executable
text is multiline text that could be executed with Wand."
  (interactive)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (save-mark-and-excursion
      (goto-char (car (bounds-of-thing-at-point 'exec-text)))
      (lexical-let ((text (~palette/ensure-prefix (string-trim-right (thing-at-point 'line))
                                                  "$ ")))
        (kill-line)
        (insert text)))))

(defun ~palette/exec-sh-in-term-mux (&optional cmd)
  "Executes a shell command in a terminal multiplexer."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~dispatch-action (concat "!!! " cmd))))

(defun ~palette/exec-sh-in-term-mux-then-pause (&optional cmd)
  "Executes a shell command in a terminal multiplexer, pauses
after command has finished running."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~dispatch-action (concat "!! " cmd))))

(defun ~palette/exec-sh-in-term-mux-piping-to-sh-output-file (&optional cmd)
  "Executes a shell command in a terminal multiplexer, piping output to the next window."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~with-output-to-buffer-file (~get-project-sh-output-path) t
      (insert (format "$ %s" cmd))
      (beginning-of-line)
      (~prepare-for-output-block t)
      (~dispatch-action (concat "!!! " (~build-|rmacs-tee-cmd cmd))))))

(defun ~palette/exec-sh-piping-to-sh-output-file (&optional cmd)
  "Executes a shell command, piping output to the next window."
  (interactive)
  (lexical-let ((cmd (~read-command-or-get-from-selection *~exec-history-path* cmd)))
    (~add-to-history-file *~exec-history-path* cmd
                          :max-history *~exec-history-max*)
    (~with-output-to-buffer-file (~get-project-sh-output-path) t
      (insert (format "$ %s" cmd))
      (beginning-of-line)
      (~exec-sh<-next-line-separate cmd))))

(defun ~palette/exec-sh-piping-here (&optional cmd)
  "TODO"
  (lexical-let ((text (thread-last (~read-command-or-get-from-selection *~exec-history-path* cmd)
                        ~palette/trim-garbage)))
    (end-of-thing 'exec-text)
    (~exec-sh<-next-line-separate text
                                  :callback #'(lambda (&rest _args)
                                                (end-of-thing 'exec-text)
                                                (forward-line)
                                                (call-interactively #'~mark-current-output-block)
                                                (call-interactively #'~ansi-colorize-region)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~ansi-colorize-current-output-block ()
  "ANSI-colorizes current output block."
  (interactive)
  (call-interactively #'~mark-current-output-block)
  (call-interactively #'~ansi-colorize-region))

(defun ~palette/point/exec-sh-in-term-mux-piping-to-sh-output-file ()
  "TODO - Add prefix, then exec"
  (interactive)
  (~palette/decorate-exec-text-at-point)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (~palette/exec-sh-in-term-mux-piping-to-sh-output-file text)))

(defun ~palette/point/exec-sh-in-term-mux-piping-here ()
  "TODO - Add prefix, then exec"
  (interactive)
  (~palette/decorate-exec-text-at-point)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (~palette/exec-sh-in-term-mux-piping-here text)))

(defun ~palette/point/exec-sh-in-term-mux ()
  "TODO - Add prefix, then exec"
  (interactive)
  (~palette/decorate-exec-text-at-point)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (~palette/exec-sh-in-term-mux text)))

(defun ~palette/point/exec-sh-in-term-mux-then-pause ()
  "TODO - Add prefix, then exec"
  (interactive)
  (~palette/decorate-exec-text-at-point)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (~palette/exec-sh-in-term-mux-then-pause text)))

(defun ~palette/point/exec-sh-piping-to-sh-output-file ()
  "TODO - Add prefix, then exec.  TODO: Customize garbage"
  (interactive)
  (~palette/decorate-exec-text-at-point)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (~palette/exec-sh-piping-to-sh-output-file text)))

(defun ~palette/point/exec-sh-piping-here ()
  "TODO"
  (interactive)
  (~palette/decorate-exec-text-at-point)
  (when-let (text (~palette/trim-garbage (thing-at-point 'exec-text)))
    (~palette/exec-sh-piping-here text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget and rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'wid-edit))
(require 'widget)

(cl-defun ui:render-widgets (&key buffer func (keep-local-vars? nil))
  "Does book-keeping and handles the render of widgets in a
buffer.  All the processing and insertions of widgets should be
in `func'."
  (interactive)
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (~clean-up-buffer :keep-local-vars? keep-local-vars?)
      (funcall func)
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (switch-to-buffer buffer))))

(cl-defun ui:render-buffer (&key buffer func (keep-local-vars? nil))
  "Does book-keeping and handles the render of text in a buffer.
All the processing and insertions of text should be in `func'.
This function is useful when building text-based interactive
application."
  (interactive)
  (let ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (~clean-up-buffer :keep-local-vars? keep-local-vars?)
      (let ((inhibit-read-only t))
        (setq truncate-lines nil)
        (setq truncate-partial-width-windows nil)
        (setq word-wrap t)
        (setq line-spacing 0)
        (setq left-fringe-width 8)
        (setq right-fringe-width 8)
        (funcall func)
        (unless view-mode
          (view-mode 1))
        (goto-char (point-min))
        (switch-to-buffer buffer)))))

(defun ui:text/title (text title-style)
  "Returns propertized text with title-like style."
  (propertize text 'face title-style))

(defun ui:text/comment (text)
  "Returns propertized text with comment-like style."
  (propertize text 'face '(:inherit (variable-pitch font-lock-comment-face))))

(defun ui:text/hline ()
  "Returns a horizontal hline."
  (propertize "\n" 'display
              `(space :align-to (- right (1)))
              'face
              '(:underline t)
              'point-entered
              'mb-kick-cursor))

(cl-defun ui:text/insert-hline (&optional (trailing-lines t))
  "Inserts a horizontal line."
  (insert (ui:text/hline))
  (when trailing-lines
    (insert "\n")))

(cl-defun ui:text/insert-line (&optional (text "") (trailing-lines t))
  "Inserts a line of text."
  (insert text)
  (when trailing-lines
    (insert "\n")))

(cl-defun ui:text/insert-title (&optional (text "")
                                        (title-style 'info-title-1)
                                        (trailing-lines t))
  "Inserts a title."
  (insert (ui:text/title text title-style))
  (when trailing-lines
    (insert "\n\n")))

(cl-defun ui:text/insert-paragraph (&optional (text "") (trailing-lines t))
  "Inserts a paragraph."
  (insert text)
  (when trailing-lines
    (insert "\n\n")))

(cl-defun ui:text/insert-comment (text &optional (trailing-lines t))
  "Inserts comment text."
  (insert (ui:text/comment text))
  (when trailing-lines
    (insert "\n\n")))

(defmacro* ui:text/insert-section (title &rest body)
  "Inserts a text section with a title."
  `(progn (ui:text/insert-title ,title 'info-title-4)
          ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple single-buffer directory browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dir-browser:record-local-dir-history (path)
  "Records directory visit history to a local variable named `local/dir-history'."
  (let ((path (expand-file-name path)))
    (setq-local local/dir-history
                (thread-last (remove path local/dir-history)
                  (cons path)))))

(defun dir-browser:render-single-entry (base-path file)
  "Renders a single file/directory entry."
  (lexical-let* ((full-path (concat base-path file))
                 (is-dir? (f-dir? full-path)))
    (insert (propertize (if is-dir?
                            (file-name-as-directory file)
                          file)
                        'keymap
                        (let ((keymap (make-sparse-keymap))
                              (is-dir? is-dir?))
                          (bind-key "<mouse-3>"
                                    #'(lambda ()
                                        (interactive)
                                        (if is-dir?
                                            (dir-browser:render-dir full-path)
                                          (~smart-open-file full-path)))
                                    keymap)
                          keymap)))
    (insert "\n")))

(defun dir-browser:render-current-path (path)
  "Renders the current working and project directories."
  (insert "Current path: ")
  (insert-text-button path
                      'keymap
                      (let ((keymap (make-sparse-keymap)))
                        (bind-key "<mouse-3>"
                                  #'(lambda ()
                                      (interactive)
                                      (kill-new path)
                                      (message "%s saved to clipboard" path))
                                  keymap)
                        keymap))
  (insert "\n")
  (insert "Project path: "
          (propertize (~get-current-project-root)
                      'keymap
                      (let ((keymap (make-sparse-keymap)))
                        (bind-key "<mouse-3>"
                                  #'(lambda ()
                                      (interactive)
                                      (dir-browser:render-dir (~get-current-project-root)))
                                  keymap)
                        keymap)))
  (insert "\n\n"))

(defun dir-browser:render-dir (path)
  "Renders the content of a directory."
  (~clean-up-buffer :keep-local-vars? t)

  (let ((inhibit-read-only t)
        (path (expand-file-name (file-name-as-directory path))))
    (dir-browser:record-local-dir-history path)
    (setq-local default-directory path)

    (dir-browser:render-current-path path)

    (dolist (file (directory-files path))
      (dir-browser:render-single-entry path file))

    (insert "\n")

    (dolist (file local/dir-history)
      (dir-browser:render-single-entry "" file)))
  (acme-mouse-mode -1)
  (evil-emacs-state))

(defun dir-browser:render-dir-buffer (path)
  "Renders a mouse-oriented buffer to browse files and directories."
  (interactive "DDirectory: ")
  (ui:render-buffer
   :buffer "*dir-browser*"
   :keep-local-vars? t
   :func
   #'(lambda ()
       (setq-local lexical-binding t)
       (let ((sym/local/dir-history (make-local-variable 'local/dir-history)))
         (unless (boundp sym/local/dir-history)
           (set sym/local/dir-history (list))))
       (dir-browser:render-dir path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alignment and indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME - Try with/out this
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode t
;;                   sh-basic-offset 4)
;;             (add-to-list 'whitespace-style 'indentation::tab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimenting with GC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmpitg/minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun cmpitg/minibuffer-exit-hook ()
  (setq gc-cons-threshold 8000000))

(add-hook 'minibuffer-setup-hook #'cmpitg/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'cmpitg/minibuffer-exit-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global keybindings should be centralized.  The need for better management
;; is greater than the need for falling back.
;;

;;
;; File navigation
;;

;; (bind-key "C-<home>" '~jekyll-add-last-updated)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QML mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package qml-mode
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple menu building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package simple-menu
;;   :load-path "/m/src/simple-menu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package racket-mode
;;   :load-path "/m/src/racket-mode/"
;;   :ensure t
;;   :commands racket-mode
;;   :mode "\\.rkt\\'"
;;   :config (progn
;;             ;; TODO: Document about indent changing and keywording
;;             (dolist (sym '(λ
;;                            ~>
;;                            ~>>
;;                            define-values
;;                            get
;;                            post
;;                            put
;;                            patch
;;                            delete
;;                            call-with-parameterization
;;                            module+))
;;               (put sym 'racket-indent-function 1)
;;               (add-to-list 'racket-keywords (~symbol->string sym))
;;               ;; (add-to-list 'racket-builtins (~symbol->string sym))
;;               )

;;             (dolist (sym '(module
;;                            module*))
;;               (put sym 'racket-indent-function 2)
;;               (add-to-list 'racket-keywords (~symbol->string sym))
;;               ;; (add-to-list 'racket-builtins (~symbol->string sym))
;;               )

;;             (add-hook 'racket-mode-hook       '~enable-paredit-mode)
;;             (add-hook 'racket-repl-mode-hook  '~enable-paredit-mode)
;;             ;; (add-hook 'racket-mode-hook       'auto-complete-mode)
;;             ;; (add-hook 'racket-repl-mode-hook  'auto-complete-mode)

;;             (bind-key "C-c C-\\" '(lambda (prefix)
;;                                     (interactive "P")
;;                                     (if prefix
;;                                         (progn (insert "(λ () )")
;;                                                (backward-char))
;;                                       (insert "λ")))
;;                       racket-mode-map)

;;             (bind-key "C-c C-b" 'racket-run racket-mode-map)
;;             (bind-key "C-c C-z" 'other-window racket-repl-mode-map)
;;             ;; (bind-key "C-c C-z" (lambda ()
;;             ;;                       (interactive)
;;             ;;                       (call-interactively 'racket-repl)
;;             ;;                       (call-interactively 'other-window))
;;             ;;           racket-mode-map)
;;             (bind-key "C-M-x" 'racket-send-definition racket-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-menu [menu-bar layout])

;; (make-submenu [menu-bar layout]
;;               :title "Layout")

;; (make-menu-item [menu-bar layout default]
;;                 :title "Default"
;;                 :action '~layout/default)

;; (make-menu-item [menu-bar layout hsplit]
;;                 :title "Horizontal split"
;;                 :action '~layout/hsplit)

;; (make-menu-item [menu-bar layout vsplit]
;;                 :title "Vertical split"
;;                 :action '~layout/vsplit)

;; (remove-menu [menu-bar literate])

;; (make-submenu [menu-bar literate]
;;               :title "Literate")

;; (make-menu-item [menu-bar literate toggle-narrow-to-region]
;;                 :title "Toggle narrowing to region"
;;                 :tooltip "Toogle narrowing to code region in Markdown mode."
;;                 :action '~toggle-narrow-to-code-region)
;; (make-menu-item [menu-bar literate narrow-to-region]
;;                 :title "Narrow to region"
;;                 :tooltip "Narrow to code region in Markdown mode."
;;                 :action '~narrow-to-code-region)
;; (make-menu-item [menu-bar literate widen-region]
;;                 :title "Widen region"
;;                 :tooltip "Widen the narrowed region, save file, an reload the buffer."
;;                 :action '~widen-narrowed-region)

;; (make-menu-separator [menu-bar literate separator] :after 'widen-region)
;; (make-menu-item [menu-bar literate generate-src]
;;                 :title "Generate source"
;;                 :tooltip "Generate source code."
;;                 :action 'ulqui:generate-src)
;; (make-menu-item [menu-bar literate generate-src-current-dir]
;;                 :title "Generate source (current dir)"
;;                 :tooltip "Generate source code from and to current directory."
;;                 :action 'ulqui:generate-src-current-dir)
;; (make-menu-item [menu-bar literate generate-html]
;;                 :title "Generate HTML"
;;                 :tooltip "Generate HTML."
;;                 :action 'ulqui:generate-html)
;; (make-menu-item [menu-bar literate generate-html-current-dir]
;;                 :title "Generate HTML (current dir)"
;;                 :tooltip "Generate HTML from and to current directory."
;;                 :action 'ulqui:generate-html-current-dir)
;; (make-menu-item [menu-bar literate generate-all]
;;                 :title "Generate all"
;;                 :tooltip "Generate source and HTML."
;;                 :action 'ulqui:generate-all)

;; (make-menu-separator [menu-bar literate seperator-2])
;; (make-menu-item [menu-bar literate srun-make]
;;                 :title "make (tmux)"
;;                 :tooltip "Run make with Tmux"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (srun "make")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu/Toolbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-menu [menu-bar toolbox])

;; (make-submenu [menu-bar toolbox]
;;               :title "Toolbox")

;; (make-menu-item [menu-bar toolbox toolbox]
;;                 :title "Visit Toolbox"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (find-file "/m/Toolbox/Toolbox.adoc")))

;; (make-submenu [menu-bar toolbox config]
;;               :title "My config")

;; (make-menu-item [menu-bar toolbox config packages]
;;                 :title "Config packages"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (find-file "~/emacs-config/load-packages.el")))

;; (make-menu-item [menu-bar toolbox config personal]
;;                 :title "Config personal stuff"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (find-file "~/emacs-config/load-personal-stuff.el")))

;; (make-submenu [menu-bar toolbox tags]
;;               :title "Tags")

;; (make-menu-item [menu-bar toolbox tags create-tags]
;;                 :title "Create tags"
;;                 :tooltip "Create a tag table from current directory."
;;                 :action '$create-tags)

;; (make-menu-item [menu-bar toolbox tags list-tags]
;;                 :title "List toolbox tags"
;;                 :action 'helm-etags-select)

;; (make-menu-item [menu-bar toolbox tags goto-tag]
;;                 :title "Go to tag"
;;                 :action 'goto-tag
;;                 :tooltip "Jump to declaration of a tag")

;; (make-menu-item [menu-bar toolbox tags goto-tag-other-window]
;;                 :title "Go to tag (other window)"
;;                 :action 'goto-tag-other-window
;;                 :tooltip "jump to declaration of a tag in another window")

;; (make-menu-separator [menu-bar toolbox separator])

;; (make-menu-item [menu-bar toolbox copy-dir]
;;                 :title "Copy directory"
;;                 :tooltip "Copy current directory to clipboard."
;;                 :action '~copy-directory)

;; (make-menu-item [menu-bar toolbox copy-file-path]
;;                 :title "Copy file path"
;;                 :tooltip "Copy current full file path to clipboard."
;;                 :action '~copy-file-path)

;; (make-menu-separator [menu-bar toolbox separator-2])

;; (make-menu-item [menu-bar toolbox xkcd]
;;                 :title "xkcd"
;;                 :action 'xkcd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package inertial-scroll :ensure t)
;; (require 'inertial-scroll)
;; (inertias-global-minor-mode 1)

;; (setq inertias-initial-velocity 50)
;; (setq inertias-friction 120)
;; (setq inertias-update-time 50)
;; (setq inertias-rest-coef 0.1)
;; (setq inertias-global-minor-mode-map
;;       (inertias-define-keymap
;;        '(
;;          ;; Mouse wheel scrolling
;;          ("<wheel-up>"   . inertias-down-wheel)
;;          ("<wheel-down>" . inertias-up-wheel)
;;          ("<mouse-4>"    . inertias-down-wheel)
;;          ("<mouse-5>"    . inertias-up-wheel)
;;          ;; Scroll keys
;;          ("<next>"  . inertias-up)
;;          ("<prior>" . inertias-down)
;;          ("C-v"     . inertias-up)
;;          ("M-v"     . inertias-down)
;;          ) inertias-prefix-key))

;;; ---
