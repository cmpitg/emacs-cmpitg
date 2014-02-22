;;
;; Copyright (C) 2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
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

;; Load before auto-complete

(use-package yasnippet
  :diminish yas-minor-mode
  :init (progn
          (add-to-list 'yas-snippet-dirs (expand-file-name *snippet-dir*))
          (yas-global-mode 1)))

;; Highlighting
(use-package hi-lock
  :commands (highlight-phrase highlight-regexp))

(use-package projectile
  :diminish projectile-mode
  :config (progn
            (projectile-global-mode)
            (setq projectile-completion-system 'grizzl)
            ;; (setq projectile-require-project-root nil)
            (setq projectile-require-project-root nil)
            (use-package helm-projectile)))

(use-package undo-tree
  :diminish undo-tree-mode)

;; For Mercurial version control system
(use-package ahg
  :commands ahg-status)

;; HTTP request library
(use-package request
  :commands request)

(use-package wand
  :init (progn
          ;; TODO

          ;;; Ideas to make use of this:
          ;;
          ;; * Window management, switch to convenient window
          ;; * Tryout Twitter Bootstrap
          ;; * SCM
          ;; * Bookmark, open file in external programs
          ;; * Controlling Firefox, evaluation JavaScript
          ;; * Live shell command
          ;; * Slide show
          ;;
          ;; Some cool Acme use cases
          ;;

          ;;
          ;; $ ls
          ;; $ cd ~; ls
          ;; file:/mnt/Data/Documents/IT/Programming Languages/Common Lisp/On Lisp.pdf
          ;; file:/mnt/Data/Documents/Human Development/The Shy Connector.ppt
          ;; https://google.com
          ;; #> message "Hello World"
          ;; mail:cmpitg@gmail.com
          ;; mail/subscribe:<listname>@lists.debian.org -> <listname>-REQUEST@lists.debian.org
          ;;


          (lambda ()
            (global-set-key (kbd "<C-return>")  'wand:execute)
            (global-set-key (kbd "<C-mouse-1>") 'wand:execute)
            (dolist (rule (list (wand:create-rule :match "\\$ "
                                                  :capture :after
                                                  :action $popup-shell-command)
                                (wand:create-rule :match "https?://"
                                                  :capture :whole
                                                  :action $open-url-in-firefox)
                                (wand:create-rule :match "file:"
                                                  :capture :after
                                                  :action toolbox:open-file)
                                (wand:create-rule :match "#> "
                                                  :capture :after
                                                  :action $add-bracket-and-eval)
                                (wand:create-rule :match "window:"
                                                  :capture :after
                                                  :action $switch-to-window)
                                (wand:create-rule :match "eshell-cd:"
                                                  :capture :after
                                                  :action $change-dir-in-eshell)
                                ))
              (wand:add-rule rule))
            (wand:create-rule :match "window:"
                              :capture :after
                              :action $switch-to-window)
            (wand:create-rule :match "eshell-cd:"
                              :capture :after
                              :action $change-dir-in-eshell))

          (setq wand:*rules*
                (list (wand:create-rule :match "\\$ "
                                        :capture :after
                                        :action ~popup-shell-command)
                      (wand:create-rule :match "https?://"
                                        :capture :whole
                                        :action ~open-url-in-firefox)
                      (wand:create-rule :match "file:"
                                        :capture :after
                                        :action toolbox:open-file)
                      (wand:create-rule :match "#> "
                                        :capture :after
                                        :action ~add-bracket-and-eval)))))

(use-package simple-menu)

;;
;; zlc - Zsh completion
;;

(use-package zlc
  :disabled t
  :init (progn
          (zlc-mode t)
          (define-key minibuffer-local-map (kbd "<down>")  'zlc-select-next-vertical)
          (define-key minibuffer-local-map (kbd "<up>")    'zlc-select-previous-vertical)
          (define-key minibuffer-local-map (kbd "<right>") 'zlc-select-next)
          (define-key minibuffer-local-map (kbd "<left>")  'zlc-select-previous)
          (define-key minibuffer-local-map (kbd "C-c")     'zlc-reset)))

(use-package tabbar-ruler
  :config (progn
            (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
            ;; (setq tabbar-ruler-global-ruler t)     ; if you want a global ruler
            ;; (setq tabbar-ruler-popup-menu t)       ; If you want a popup menu.
            ;; (setq tabbar-ruler-popup-toolbar t)    ; If you want a popup toolbar
            ;; (setq tabbar-ruler-popup-scrollbar t)) ; If you want to only show
            ;;                                        ; the scroll bar when your
            ;;                                        ; mouse is moving.
            ))

(use-package sunrise-commander
  :disabled t
  :init (progn
          (setq sr-listing-switches
                " --time-style=locale --group-directories-first -alDhgG")

          ;; Display modeline using UTF8 characters
          (setq sr-modeline-use-utf8-marks t)

          ;; Prefer Sunrise over Dired
          ;; (setq find-directory-functions (cons cvs-dired-noselect dired-noselect))
          ;; (setq-default find-directory-functions (cons 'sr-dired find-directory-functions))
          ;; (setq find-directory-functions (cons 'sr-dired find-directory-functions))
          ))

;;
;; Slime for Common Lisp development
;;

(use-package slime
  :commands common-lisp-mode
  :config (progn
            (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
            (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
            (setenv "SBCL_HOME" "/home/cmpitg/opt/sbcl")
            (setq inferior-lisp-program *default-lisp-repl-path*)
            (slime-setup)))

;;
;; Ruby with Pry and Rsense
;;
;; https://github.com/Mon-Ouie/ruby-dev.el

(use-package ruby-mode
  :commands ruby-mode
  :init (progn
          (~auto-load-mode '("\\Rakefile$" "\\.mab$") 'ruby-mode))
  :config (progn
            (use-package ruby-dev
              :init (progn
                      (autoload 'turn-on-ruby-dev "ruby-dev" nil t)

                      (add-hook 'ruby-mode-hook 'turn-on-ruby-dev)))))

;; (use-package rsense
;;   :init (progn
;;           (setq rsense-home (getenv "$RSENSE_HOME"))))

(use-package recentf
  :defer t
  :config (progn
            (recentf-mode 1)
            (setq recentf-max-menu-items 128)))

(use-package python
  :config (progn
            ;; Workaround: virtualenvwrapper.el needs to be loaded explicitly
            (progn
              (~load-files (buffer-file-name (find-library "virtualenvwrapper")))
              (kill-buffer "virtualenvwrapper.el"))
            (use-package virtualenvwrapper
              :config (progn 
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell)
                        (setq venv-location "/m/virtenvs/")))))

(use-package jedi
  :disabled t
  :config (progn
            (add-hook 'python-mode-hook 'jedi:setup)
            (setq jedi:setup-keys t)
            (setq jedi:complete-on-dot t)))

(use-package picolisp
  :mode ("\\.l$" . picolisp-mode)
  :config (progn
            (setq picolisp-program-name "~/opt/picolisp/bin/plmod")
            (add-hook 'picolisp-mode-hook
                      (lambda ()
                        (paredit-mode +1)
                        (tsm-mode)
                        (define-key picolisp-mode-map (kbd "RET") 'newline-and-indent)
                        (define-key picolisp-mode-map (kbd "C-h") 'paredit-backward-delete)))))

;;
;; Auto pairing brackets
;;

(use-package smartparens-config
  :config (progn
            (eval-after-load 'smartparens
              '(progn
                 ;; (defadvice smartparens-mode (around disable-autopairs-around (arg))
                 ;;   "Disable autopairs mode if smartparens-mode is turned on."
                 ;;   ad-do-it
                 ;;   (autopair-mode 0))

                 ;; (ad-activate 'smartparens-mode)

                 (smartparens-global-mode)))))

(use-package paredit
  :config (progn
            (defadvice paredit-mode (around disable-otherparenslib-around (arg))
              "Disable autopairs mode if paredit-mode is turned on."
              ad-do-it
              (cond ((null ad-return-value)
                     (smartparens-mode 1))
                    (t
                     (smartparens-mode 0))))

            (ad-activate 'paredit-mode)

            ;; Use with SLIME REPL
            (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

            ;; Stop SLIME's REPL from grabbing DEL,
            ;; which is annoying when backspacing over a '('
            (defun override-slime-repl-bindings-with-paredit ()
              (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))

            (add-hook 'slime-repl-mode-hook
                      'override-slime-repl-bindings-with-paredit)

            (add-hook 'emacs-lisp-mode-hook       '~load-paredit-mode)
            (add-hook 'lisp-mode-hook             '~load-paredit-mode)
            (add-hook 'lisp-interaction-mode-hook '~load-paredit-mode)
            (add-hook 'scheme-mode-hook           '~load-paredit-mode)))


;;
;; Predictive abbreviation
;;

(use-package pabbrev
  :disabled t
  :config (progn
            (global-pabbrev-mode)))

(use-package moz
  :config (progn
            (add-hook 'javascript-mode-hook '-setup-moz-javascript)
            (add-hook 'js3-mode-hook        '-setup-moz-javascript)))

(use-package monky
  :config (progn
            (setq monky-process-type 'cmdserver)))

(use-package markdown-mode
  :init (progn
          (~auto-load-mode '("\\.md$" "\\.markdown$") 'markdown-mode))
  :config (progn
            (use-package markdown-mode+)
            (add-hook 'markdown-mode-hook 'auto-fill-mode)))

;; (eval-after-load 'js3-mode)
;; (setq js3-auto-indent-p t
;;       js3-enter-indents-newline t
;;       js3-indent-on-enter-key t)

(use-package js2-mode
  :mode "\\.js" 
  :interpreter "node"
  :config (progn
            (add-hook 'js-mode-hook 'js2-minor-mode)
            ;; (~auto-load-mode '("\\.js\\'") 'js2-mode)
            ;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

            (add-hook 'html-mode-hook '~auto-reload-firefox-after-save-hook)
            (add-hook 'css-mode-hook '~auto-reload-firefox-after-save-hook)))

(use-package ibus
  :config (progn
            ;; Use C-SPC for Set Mark command
            (ibus-define-common-key ?\C-\s nil)
            ;; Use C-/ for Undo command
            (ibus-define-common-key ?\C-/ nil)
            ;; Change cursor color depending on IBus status
            (setq ibus-cursor-color '("red" "blue" "limegreen"))
            (setq ibus-agent-file-name (~get-local-config-dir "local-packages/ibus.el/ibus-el-agent"))))

(use-package haskell-mode
  :disabled t
  :init (progn
          
          )
  :config (progn
            (use-package inf-haskell)
            (use-package hs-lint)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
            ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
            ))

(use-package eshell
  :init (progn
          (use-package exec-path-from-shell))
  :config (progn
            (setq eshell-prefer-lisp-functions t)

            ;; Eshell smart mode
            (require 'em-smart)
            (setq eshell-where-to-jump 'begin)
            (setq eshell-review-quick-commands nil)
            (setq eshell-smart-space-goes-to-end t)

            (setq eshell-aliases-file (~get-local-config-dir "misc/eshell-aliases.el"))

            ;; Auto complete support
            (defun ac-pcomplete ()
              ;; eshell uses `insert-and-inherit' to insert a \t if no completion
              ;; can be found, but this must not happen as auto-complete source
              (flet ((insert-and-inherit (&rest args)))
                ;; this code is stolen from `pcomplete' in pcomplete.el
                (let* (tramp-mode ;; do not automatically complete remote stuff
                       (pcomplete-stub)
                       (pcomplete-show-list t) ;; inhibit patterns like * being deleted
                       pcomplete-seen pcomplete-norm-func
                       pcomplete-args pcomplete-last pcomplete-index
                       (pcomplete-autolist pcomplete-autolist)
                       (pcomplete-suffix-list pcomplete-suffix-list)
                       (candidates (pcomplete-completions))
                       (beg (pcomplete-begin))
                       ;; note, buffer text and completion argument may be
                       ;; different because the buffer text may bet transformed
                       ;; before being completed (e.g. variables like $HOME may be
                       ;; expanded)
                       (buftext (buffer-substring beg (point)))
                       (arg (nth pcomplete-index pcomplete-args)))
                  ;; we auto-complete only if the stub is non-empty and matches
                  ;; the end of the buffer text
                  (when (and (not (zerop (length pcomplete-stub)))
                             (or (string= pcomplete-stub ; Emacs 23
                                          (substring buftext
                                                     (max 0
                                                          (- (length buftext)
                                                             (length pcomplete-stub)))))
                                 (string= pcomplete-stub ; Emacs 24
                                          (substring arg
                                                     (max 0
                                                          (- (length arg)
                                                             (length pcomplete-stub)))))))
                    ;; Collect all possible completions for the stub. Note that
                    ;; `candidates` may be a function, that's why we use
                    ;; `all-completions`.
                    (let* ((cnds (all-completions pcomplete-stub candidates))
                           (bnds (completion-boundaries pcomplete-stub
                                                        candidates
                                                        nil
                                                        ""))
                           (skip (- (length pcomplete-stub) (car bnds))))
                      ;; We replace the stub at the beginning of each candidate by
                      ;; the real buffer content.
                      (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                              cnds))))))

            (defvar ac-source-pcomplete
              '((candidates . ac-pcomplete)))

            (eval-after-load 'auto-complete
	      '(progn
		 (add-to-list 'ac-modes 'eshell-mode)))

            (add-hook 'eshell-mode-hook
                      (lambda ()
                        (setq ac-sources '(ac-source-pcomplete))

                        (add-to-list 'eshell-visual-commands "mc")
                        (add-to-list 'eshell-visual-commands "ranger")
                        (add-to-list 'eshell-visual-commands "git log")
                        (bind-key "s-d" 'eshell-bol            eshell-mode-map)
                        (bind-key "s-C" 'eshell-previous-input eshell-mode-map)
                        (bind-key "s-T" 'eshell-next-input     eshell-mode-map)
                        (bind-key "<S-mouse-1>" '~insert-text-at-the-end
                                  eshell-mode-map)))
               
            ;; Read $PATH variable
            (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize))))

(use-package eldoc
  :config (progn
            (eldoc-add-command 'paredit-backward-delete
                               'paredit-close-round)

            (add-hook 'emacs-lisp-mode-hook        'turn-on-eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook  'turn-on-eldoc-mode)
            (add-hook 'ielm-mode-hook              'turn-on-eldoc-mode)))

;; Default: -lahF

(use-package dired-details+ 
  :init (progn
          (setq dired-listing-switches "-lhFgG --group-directories-first")))

(use-package auto-complete
  :diminish auto-complete-mode
  :init (progn
          (require 'auto-complete-config)
          (ac-config-default)
          (setq ac-sources
                '(ac-source-filename
                  ac-source-functions
                  ;; ac-source-yasnippet
                  ac-source-variables
                  ac-source-symbols
                  ac-source-features
                  ac-source-abbrev
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))

          (auto-complete-mode 1)
          (setq ac-fuzzy-enable t)

          (add-hook 'ruby-mode-hook
                    (lambda ()
                      (add-to-list 'ac-sources 'ac-source-rsense-method)
                      (add-to-list 'ac-sources 'ac-source-rsense-constant)))))

(~load-config-files "local-packages/ack-and-a-half/ack-and-a-half.el")

(use-package ack-and-a-half
  :init (progn
          ;; Fix Debian-based distros' executable file
          (setq ack-and-a-half-executable (or (executable-find "ack-grep")
                                              (executable-find "ack")))
          (defalias 'ack 'ack-and-a-half)
          (defalias 'ack-same 'ack-and-a-half-same)
          (defalias 'ack-find-file 'ack-and-a-half-find-file)
          (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

(use-package yaml-mode
  :commands yaml-mode)

(use-package go-mode
  :commands go-mode)

(use-package gist
  :defer t)

(use-package json
  :defer t)

(use-package magit
  :commands magit-status
  :config (progn
            (set-face-foreground 'magit-diff-add "black")
            (set-face-background 'magit-diff-add "yellow2")
            (set-face-foreground 'magit-diff-none "brown")
            (set-face-background 'magit-diff-none "gray")
            (set-face-foreground 'magit-diff-del "red3")
            (when (not window-system)
              (set-face-background 'magit-item-highlight "black"))))

;; Quack doc: http://www.neilvandyke.org/quack/quack.el
;; Geiser doc: http://www.nongnu.org/geiser

(use-package geiser
  :commands geiser-mode
  :init (progn
          (setq geiser-default-implementation "racket"))
  :config (progn
            (add-hook 'geiser-repl-mode-hook   '~load-paredit-mode)

            ;; Auto-complete backend
            (use-package ac-geiser
              :init (progn
                      (add-hook 'geiser-mode-hook        'ac-geiser-setup)
                      (add-hook 'geiser-repl-mode-hook   'ac-geiser-setup)
                      (eval-after-load 'auto-complete
                        '(add-to-list 'ac-modes 'geiser-repl-mode))))

            (eval-after-load 'geiser-mode
              '(progn
                 (dolist (sym '(λ
                                ~>
                                ~>>
                                define-values
                                get
                                post
                                put
                                patch
                                delete
                                call-with-parameterization
                                module+))
                   (put sym 'scheme-indent-function 1)
                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

                 (dolist (sym '(module
                                module*))
                   (put sym 'scheme-indent-function 2)
                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

                 (put '{ 'scheme-indent-function 0)
                 (put (~string->symbol "[") 'scheme-indent-function 0)

                 (defadvice geiser-eval-region (after send-region-to (&rest arg))
                   ;; ad-do-it
                   (let ((start (ad-get-arg 0))
                         (end   (ad-get-arg 1)))
                     (~geiser-send-string (~get-text start end))))

                 ;; (ad-deactivate 'geiser-eval-region)
                 (ad-activate 'geiser-eval-region)))))

;; Load after Geiser
(use-package quack)

(use-package racket-mode
  :disabled t
  :config (progn
            ;; TODO: Document about indent changing and keywording
            (put 'λ 'racket-indent-function 1)
            (add-to-list 'racket-keywords "λ")
            (bind-key "C-c C-b" 'racket-run racket-mode-map)
            (bind-key "C-c C-z" 'other-window racket-repl-mode-map)
            (bind-key "C-c C-z" (lambda ()
                                  (interactive)
                                  (call-interactively 'racket-repl)
                                  (call-interactively 'other-window))
                      racket-mode-map)
            (bind-key "C-M-x" 'racket-send-definition racket-mode-map)))

(use-package cider
  :config (progn
            (use-package clojure-mode)
            (use-package clojure-cheatsheet)
            (use-package clojure-test-mode)
            (use-package clojurescript-mode)
            (use-package ac-nrepl)

            (add-hook 'clojure-mode-hook 'cider-mode)
     
            (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
            (add-hook 'cider-repl-mode-hook 'paredit-mode)

            ;; ;; Moving inside subword
            (add-hook 'cider-repl-mode-hook 'subword-mode)

            ;; Hide *nrepl-connection* and *nrepl-server*
            (setq nrepl-hide-special-buffers t)

            ;; Prevent the auto-display of the REPL buffer in a separate window after
            ;; connection is established
            ;; (setq cider-repl-pop-to-buffer-on-connect nil)
            (setq cider-repl-pop-to-buffer-on-connect t)

            ;; the REPL
            (setq cider-popup-stacktraces nil)

            ;; Enable error buffer popping also in the REPL
            (setq cider-repl-popup-stacktraces t)

            (setq nrepl-buffer-name-separator "-")
            (setq nrepl-buffer-name-show-port t)

            (setq cider-repl-history-size 4096)

            ;; Autocomplete nREPL
            (require 'ac-nrepl)
            (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
            (add-hook 'cider-mode-hook 'ac-nrepl-setup)
            (eval-after-load 'auto-complete
              '(progn
                 (add-to-list 'ac-modes 'cider-repl-mode)
                 (defun set-auto-complete-as-completion-at-point-function ()
                   (setq completion-at-point-functions '(auto-complete)))
                 (add-hook 'auto-complete-mode-hook
                           'set-auto-complete-as-completion-at-point-function)
                 (add-hook 'cider-repl-mode-hook
                           'set-auto-complete-as-completion-at-point-function)
                 (add-hook 'cider-mode-hook
                           'set-auto-complete-as-completion-at-point-function)))))

;; Hyde.el to manage Jekyll blog
;;   https://github.com/nibrahim/Hyde
;; Workaround for bad package management
(let ((local-elpa-path (expand-file-name "~/.emacs.d/elpa/")))
  (-> (directory-files local-elpa-path nil "hyde-.*")
    first
    (s-concat "/hyde-md.el")
    (s-append local-elpa-path)
    ~load-files)
  (use-package hyde
    :commands hyde))

(use-package scratch-ext)

;; HTTP REPL FTW
;; Again, bad packaging needs workaround
(~load-files (~get-library-full-path "httprepl"))
(use-package httprepl)

(use-package w3m
  :commands w3m-browse-url
  :init (progn
          (setq browse-url-browser-function 'w3m-browse-url)))

(use-package multi-scratch
  :commands multi-scratch-new)

(use-package later-do
  :defer t)

(use-package whitespace
  :defer t)
