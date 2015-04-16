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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Visit anything.gpg and it will encrypt it when you save the buffer.
;;
;; To prevent EPG from prompting for a key every time you save a file, put the
;; following at the top of your file:
;;
;;    -*- epa-file-encrypt-to: ("your@email.address") -*-

(use-package epa-file
  :ensure epa
  :config (progn
            (epa-file-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control systems: Mercurial and Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ahg
  :ensure ahg
  :commands ahg-status)

(use-package magit
  :ensure magit
  :commands (magit-status ~get-scm magit-get-top-dir)
  :config (progn
            (set-face-foreground 'magit-diff-add "black")
            (set-face-background 'magit-diff-add "yellow2")
            (set-face-foreground 'magit-diff-none "brown")
            (set-face-background 'magit-diff-none "gray")
            (set-face-foreground 'magit-diff-del "red3")
            (when (not window-system)
              (set-face-background 'magit-item-highlight "black"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP request library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/tkf/emacs-request
;;
;; Examples
;;
;; (request
;;  "http://httpbin.org/get"
;;  :params '(("key" . "value") ("key2" . "value2"))
;;  :parser 'json-read
;;  :success (function*
;;            (lambda (&key data &allow-other-keys)
;;              (message "I sent: %S" (assoc-default 'args data)))))
;; (request
;;  "http://httpbin.org/post"
;;  :type "POST"
;;  :data '(("key" . "value") ("key2" . "value2"))
;;  ;; :data "key=value&key2=value2"  ; this is equivalent
;;  :parser 'json-read
;;  :success (function*
;;            (lambda (&key data &allow-other-keys)
;;              (message "I sent: %S" (assoc-default 'form data)))))

(use-package request
  :ensure request
  :commands request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package racket-mode
  :load-path "/m/src/racket-mode/"
  :commands racket-mode
  :mode "\\.rkt\\'"
  :config (progn
            ;; TODO: Document about indent changing and keywording
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
              (put sym 'racket-indent-function 1)
              (add-to-list 'racket-keywords (~symbol->string sym))
              (add-to-list 'racket-builtins (~symbol->string sym)))

            (dolist (sym '(module
                           module*))
              (put sym 'racket-indent-function 2)
              (add-to-list 'racket-keywords (~symbol->string sym))
              (add-to-list 'racket-builtins (~symbol->string sym)))

            (add-hook 'racket-mode-hook       '~load-paredit-mode)
            (add-hook 'racket-repl-mode-hook  '~load-paredit-mode)
            (add-hook 'racket-mode-hook       'auto-complete-mode)
            (add-hook 'racket-repl-mode-hook  'auto-complete-mode)

            (bind-key "C-c C-\\" '(lambda (prefix)
                                    (interactive "P")
                                    (if prefix
                                        (progn (insert "(λ () )")
                                               (backward-char))
                                      (insert "λ")))
                      racket-mode-map)

            (bind-key "C-c C-b" 'racket-run racket-mode-map)
            (bind-key "C-c C-z" 'other-window racket-repl-mode-map)
            ;; (bind-key "C-c C-z" (lambda ()
            ;;                       (interactive)
            ;;                       (call-interactively 'racket-repl)
            ;;                       (call-interactively 'other-window))
            ;;           racket-mode-map)
            (bind-key "C-M-x" 'racket-send-definition racket-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quack doc: http://www.neilvandyke.org/quack/quack.el
;; Geiser doc: http://www.nongnu.org/geiser

;; (use-package geiser
;;   :disabled t
;;   :config (progn
;;             (setq geiser-default-implementation "racket")
;;             (add-hook 'geiser-repl-mode-hook   '~load-paredit-mode)

;;             ;; Auto-complete backend
;;             (use-package ac-geiser
;;               :init (progn
;;                       (add-hook 'geiser-mode-hook        'ac-geiser-setup)
;;                       (add-hook 'geiser-repl-mode-hook   'ac-geiser-setup)
;;                       (eval-after-load 'auto-complete
;;                         '(add-to-list 'ac-modes 'geiser-repl-mode))))

;;             (eval-after-load 'geiser-mode
;;               '(progn
;;                 (dolist (sym '(λ
;;                                ~>
;;                                ~>>
;;                                define-values
;;                                get
;;                                post
;;                                put
;;                                patch
;;                                delete
;;                                call-with-parameterization
;;                                module+))
;;                   (put sym 'scheme-indent-function 1)
;;                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;                 (dolist (sym '(with-shell-commands))
;;                   (put sym 'scheme-indent-function 0)
;;                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;                 (dolist (sym '(module
;;                                module*))
;;                   (put sym 'scheme-indent-function 2)
;;                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;                 (put '{ 'scheme-indent-function 0)
;;                 (put (~string->symbol "[") 'scheme-indent-function 0)

;;                 (defadvice geiser-eval-region (after send-region-to (&rest arg))
;;                  ;; ad-do-it
;;                  (let ((start (ad-get-arg 0))
;;                        (end   (ad-get-arg 1)))
;;                    (~geiser-send-string (~get-text start end))))

;;                 ;; (ad-deactivate 'geiser-eval-region)
;;                 (ad-activate 'geiser-eval-region)))))

;; Load after Geiser
;; (use-package quack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode2
  :ensure scala-mode2
  :commands scala-mode2
  :init
  (progn
    (use-package sbt-mode
      :ensure sbt-mode
      :config
      (progn
        (add-hook 'sbt-mode-hook
                  (lambda ()
                    ;; compilation-skip-threshold tells the compilation minor-mode
                    ;; which type of compiler output can be skipped. 1 = skip info
                    ;; 2 = skip info and warnings.
                    (setq compilation-skip-threshold 1)

                    (bind-key "C-a" 'comint-bol sbt-mode-map)
                    (bind-key "s-d" 'comint-bol sbt-mode-map)
                    (bind-key "M-RET" 'comint-accumulate sbt-mode-map)))
        (add-hook 'scala-mode-hook
                  (lambda ()
                    (bind-key "M-." 'sbt-find-definitions scala-mode-map)
                    (bind-key "C-x '" 'sbt-run-previous-command scala-mode-map)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package slime
  :ensure slime
  :commands common-lisp-mode
  :config (progn
            (add-hook 'lisp-mode-hook
                      (lambda ()
                        (slime-mode t)
                        (bind-key "<f1>" 'slime-hyperspec-lookup lisp-mode-map)))

            (add-hook 'inferior-lisp-mode-hook
                      (lambda ()
                        (inferior-slime-mode t)
                        (~load-paredit-mode)
                        (eldoc-mode 1)))

            (setenv "SBCL_HOME" "/usr/local/lib/sbcl/")
            (setenv "XDG_DATA_DIRS" "/usr/share/i3:/usr/local/share:/usr/share")

            (setq inferior-lisp-program "/usr/local/bin/sbcl")

            (setq slime-lisp-implementations
                  '((sbcl  ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
                    (clisp ("/usr/bin/clisp" "-q -I"))))

            (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

            (font-lock-add-keywords 'emacs-lisp-mode
                                    '(("defroute" . font-lock-keyword-face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julia development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package julia-mode
  :ensure julia-mode
  :commands julia-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package clojure-mode
  :ensure clojure-mode
  :commands clojure-mode
  :mode "\\.clj\\'"
  :config
  (progn
    (use-package cider
      :ensure cider
      :config (progn
                (use-package clojure-cheatsheet
                  :ensure clojure-cheatsheet)

                (use-package clojurescript-mode
                  :ensure clojurescript-mode)

                (use-package ac-cider
                  :ensure ac-cider)

                (add-hook 'clojure-mode-hook 'cider-mode)
                (add-hook 'clojure-mode-hook '~load-paredit-mode)

                (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
                (add-hook 'cider-repl-mode-hook '~load-paredit-mode)

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
                     'set-auto-complete-as-completion-at-point-function)))

                ;; Clojure latest library
                ;;   https://github.com/AdamClements/latest-clojure-libraries

                (use-package latest-clojure-libraries
                  :ensure latest-clojure-libraries
                  :init (progn
                          (defalias '~clojure-insert-latest-dependency
                            'latest-clojure-libraries-insert-dependency)))

                (define-clojure-indent
                  (defroutes 'defun)
                  (GET 2)
                  (POST 2)
                  (PUT 2)
                  (DELETE 2)
                  (HEAD 2)
                  (ANY 2)
                  (context 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WispJS development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wispjs-mode
  :ensure wispjs-mode
  :commands wispjs-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/Mon-Ouie/ruby-dev.el

(use-package ruby-mode
  :ensure ruby-mode
  :commands ruby-mode
  :init (progn
          (~auto-load-mode '("\\Rakefile$" "\\.mab$") 'ruby-mode))
  :config (progn
            (use-package ruby-dev
              :ensure ruby-dev
              :init (progn
                      (autoload 'turn-on-ruby-dev "ruby-dev" nil t)
                      (add-hook 'ruby-mode-hook 'turn-on-ruby-dev)))))

;; (use-package rsense
;;   :init (progn
;;           (setq rsense-home (getenv "$RSENSE_HOME"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delphi and Pascal development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package delphi
  :commands delphi-mode
  :config (progn
            (~auto-load-mode '("\\.pas$" "\\.pp$") 'delphi-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in

(use-package python
  :config (progn
            (~auto-load-mode '("\\.py$") 'python-mode)
            ;; Workaround: virtualenvwrapper.el needs to be loaded explicitly
            (progn
              (~elpa-install 'virtualenvwrapper)
              (load-file (~get-library-full-path "virtualenvwrapper"))
              ;; (kill-buffer "virtualenvwrapper.el")
              )
            (use-package virtualenvwrapper
              :config (progn
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell)
                        (setq venv-location "/m/virtenvs/")))))

(use-package django-mode
  :ensure django-mode
  :commands python-mode
  :config (progn
            (use-package python-django
              :ensure python-django)
            (use-package django-html-mode
              :mode "\\\\.djhtml$")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config (progn
            (add-hook 'js-mode-hook 'js2-minor-mode)
            ;; (~auto-load-mode '("\\.js\\'") 'js2-mode)
            ;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

            (add-hook 'html-mode-hook '~auto-reload-firefox-after-save-hook)
            (add-hook 'css-mode-hook '~auto-reload-firefox-after-save-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :ensure haskell-mode
  :commands haskell-mode
  :mode "\\.hs\\'"
  :config (progn
            (use-package inf-haskell)
            ;; (use-package hs-lint)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
            ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package toml-mode
  :ensure toml-mode
  :commands toml-mode
  :mode "\\.toml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibus bridge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (~get-config "local-packages/ibus.el/ibus-dev.el"))
(use-package ibus
  :commands ibus-mode
  :config (progn
            ;; Use C-SPC for Set Mark command
            (ibus-define-common-key ?\C-\s nil)
            ;; Use C-/ for Undo command
            (ibus-define-common-key ?\C-/ nil)
            ;; Change cursor color depending on IBus status
            (setq ibus-cursor-color '("red" "blue" "limegreen"))
            (setq ibus-agent-file-name (~get-config "local-packages/ibus.el/ibus-el-agent"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open last session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package save-visited-files
  :ensure save-visited-files
  :config (progn
            (turn-on-save-visited-files-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File browsing with neotree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Neotree breaks tab completion in minibuffer

;; (use-package neotree
;;   :ensure neotree
;;   :init (progn
;;           (bind-key "<M-f12>" 'neotree-toggle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eshell
  :commands eshell
  :init (progn
          (use-package exec-path-from-shell
            :ensure exec-path-from-shell)

          ;; ElDoc in Eshell
          (defadvice eldoc-current-symbol
            (around eldoc-current-symbol activate)
            ad-do-it
            (if (and (not ad-return-value)
                     (eq major-mode 'eshell-mode))
                (save-excursion
                  (goto-char eshell-last-output-end)
                  (let ((esym (eshell-find-alias-function (current-word)))
                        (sym (intern-soft (current-word))))
                    (setq ad-return-value (or esym sym)))))))
  :config (progn
            (add-hook 'eshell-mode-hook 'eldoc-mode)

            (setq eshell-prefer-lisp-functions t)

            ;; Eshell smart mode
            (require 'em-smart)
            (setq eshell-where-to-jump 'begin)
            (setq eshell-review-quick-commands nil)
            (setq eshell-smart-space-goes-to-end t)

            (setq eshell-aliases-file (~get-config "misc/eshell-aliases.el"))

            ;; Beginning of command line, not line
            (defun eshell-beginning-of-command-line ()
              "Move to beginning of command line, not line."
              (interactive)
              (let ((p (point)))
                (beginning-of-line)
                (loop do (forward-char)
                      until (equal (current-char) " "))
                (forward-char)))

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
                        ;; (bind-key "s-d" 'eshell-beginning-of-command-line eshell-mode-map)
                        (bind-key "s-d" 'eshell-bol            eshell-mode-map)
                        (bind-key "s-C" 'eshell-previous-input eshell-mode-map)
                        (bind-key "s-T" 'eshell-next-input     eshell-mode-map)
                        (bind-key "<S-mouse-1>" '~insert-text-at-the-end
                                  eshell-mode-map)))

            (setq eshell-prompt-function
                  (lambda ()
                    (let* ((username (getenv "USER"))
                           (hostname (getenv "HOST"))
                           (time     (format-time-string "%Y/%m/%d %H:%M"))
                           (pwd      (eshell/pwd)))
                      (concat "--- " time " " username "@" hostname " " pwd " ---"
                              "\n"
                              "$ "))))
            
            (setq eshell-prompt-regexp "^[#$] ")

            ;; Read $PATH variable
            (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ee:cmpitg-flavored-packages)
