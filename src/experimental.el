;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2014-2022 Ha-Duong Nguyen (@cmpitg)
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
;; Emacs, don't move my window
;; Ref: https://www.reddit.com/r/emacs/comments/llvyxe/hey_emacs_dont_move_my_windows_customizing/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq display-buffer-alist
;;       '((".*"
;;          (display-buffer-reuse-window display-buffer-same-window)
;;          ;; (reusable-frames . t)
;;          )))

;; ;; display-buffer: avoid resizing
;; (setq even-window-sizes nil)

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

;; (use-package posframe
;;   :ensure t)

;; ;; TODO: Review all the custom-set-variables -> customize-set-variable
;; ;; TODO: https://github.com/abo-abo/hydra/blob/master/hydra.el - read docs thoroughly
;; ;; TODO: Use defhydra+
;; ;; TODO: Check out https://github.com/honmaple/emacs-maple-minibuffer
;; ;; TODO: Check out https://github.com/tumashu/ivy-posframe
;; ;; TODO: Ref https://github.com/tumashu/posframe
;; ;; TODO: Check out https://www.reddit.com/r/emacs/comments/cdfr20/ann_majormodehydra_prettyhydra_020/
;; ;; TODO: Emacs Lucid's set-frame-position is too slow: https://github.com/tumashu/posframe/issues/45
;; ;; TODO: Check out https://git.savannah.gnu.org/cgit/emacs.git/commit/?h=emacs-27&id=c49d379f17bcb0ce82604def2eaa04bda00bd5ec
;; ;; TODO: Check out https://github.com/tumashu/company-posframe/issues/2
;; ;; TODO: Check out https://github.com/abo-abo/hydra/wiki/Hydras-by-Topic

;; (customize-set-variable 'hydra-hint-display-type 'posframe)
;; (customize-set-variable 'hydra-hint-display-type 'lv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using Hydra in Posframe
;; Ref: https://github.com/Ladicle/hydra-posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package hydra-posframe
;;   :straight
;;   (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
;;   :after (posframe)
;;   :config
;;   (progn
;;     (hydra-posframe-mode)
;;     (customize-set-variable 'hydra-posframe-border-width 3)
;;     (customize-set-variable 'hydra-posframe-border-face 'black)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell extras
;; Ref: https://github.com/riscy/shx-for-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package shx
  :ensure t
  :init
  (progn
    (shx-global-mode 1))
  :config
  (progn
    ;; Break up long lines
    (setq shx-max-output 4096)

    ;; shx prompt
    (setq shx-leader ":")))

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
