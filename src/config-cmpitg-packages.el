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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control systems: Mercurial and Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package monky
  :ensure t
  :commands monky-status)

(use-package magit
  :ensure t
  :commands (magit-status ~get-scm magit-get-top-dir)
  :init (progn
          (setf magit-push-always-verify 'pp)
          ;; (setf git-commit-check-style-conventions nil)
          (setf git-commit-finish-query-functions nil)))

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
  :ensure t
  :commands request)

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
;; Shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sh-mode-hook (lambda ()
                          (setq indent-tabs-mode t
                                sh-basic-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/tjarvstrand/edts
;;   edts-man-setup to setup documentation
;;   Make sure you have `rebar' 2.0.1+ installed, then
;;       cd ~/.emacs.d/elpa/edts*/
;;       find . -name rebar -print0 | xargs -0 -I{} echo "{}"
;;       make

(use-package edts-start
  :ensure t
  :disabled t
  :config (progn
            (setq erlang-electric-commands '(erlang-electric-comma
                                             erlang-electric-semicolon))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode2
  :ensure t
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
;; Julia development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package julia-mode
  :ensure t
  :commands julia-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://cider.readthedocs.io/en/latest/
;; https://github.com/clojure-emacs/cider/blob/master/doc/configuration.md
;;

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :config
  (progn
    (use-package cider
      :ensure cider
      :config
      (progn
        ;; https://github.com/clojure-emacs/helm-cider
        (use-package helm-cider
          :ensure t
          :config (helm-cider-mode 1))

        (use-package clojure-cheatsheet
          :ensure t)

        (use-package clojurescript-mode
          :ensure t)

        (use-package midje-mode
          :ensure t
          :diminish midje-mode)

        ;; (add-hook 'clojure-mode-hook 'cider-mode)
        (add-hook 'clojure-mode-hook '~load-paredit-mode)
        (add-hook 'clojure-mode-hook 'midje-mode)

        ;; Only display eldoc for current function/macro, not current symbol
        (setq cider-eldoc-display-for-symbol-at-point nil)
        (add-hook 'cider-mode-hook 'eldoc-mode)

        (add-hook 'cider-repl-mode-hook '~load-paredit-mode)

        ;; ;; Moving inside subword
        (add-hook 'cider-repl-mode-hook 'subword-mode)

        ;; Hide *nrepl-connection* and *nrepl-server*
        (setq nrepl-hide-special-buffers t)
        ;; Prevent the auto-display of the REPL buffer in a separate window after
        ;; connection is established
        ;; (setq cider-repl-pop-to-buffer-on-connect nil)
        (setq cider-repl-pop-to-buffer-on-connect t)
        (setq cider-popup-stacktraces nil)
        ;; Enable error buffer popping also in the REPL
        (setq cider-repl-popup-stacktraces t)

        (setq nrepl-buffer-name-separator "-")
        (setq nrepl-buffer-name-show-port t)

        (setq cider-repl-history-size 9999)

        ;; Clojure docs lookup
        (use-package cider-grimoire)

        (use-package clj-refactor
          :ensure t
          :config
          (progn
            (defun my/clojure-refactor-hooking ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-l"))

            (add-hook 'clojure-mode-hook #'my/clojure-refactor-hooking)))

        (define-clojure-indent
          (defroutes 'defun)
          (GET 2)
          (POST 2)
          (PUT 2)
          (DELETE 2)
          (HEAD 2)
          (ANY 2)
          (context 2)
          (tabular '(2 1))
          (are '(2 1)))
        
        (defun ~cider-eval (&optional expr)
          "Eval an expression in Cider REPL."
          (interactive)
          (let ((expr (cond ((not (~string-empty? expr))
                             expr)
                            (t
                             (~read-string "Expression: "))))
                (cider-buffer (->> (buffer-list)
                                   (-filter (lambda (buffer)
                                              (string-match-p "\\*cider-repl.*"
                                                              (buffer-name buffer))))
                                   first)))
            (unless (null cider-buffer)
              (set-buffer cider-buffer)
              (goto-char (point-max))
              (insert expr)
              (cider-repl-return)
              (~popup-buffer (buffer-name cider-buffer)))))

        (defun ~cider-compile-file-and-run-main ()
          "Compile current file with Cider and run the `-main' function."
          (interactive)
          (call-interactively 'cider-load-current-buffer)
          (~cider-eval "(-main)"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/Mon-Ouie/ruby-dev.el

(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  :mode (("\\Rakefile$" . ruby-mode)
         ("\\.mab$"     . ruby-mode))
  :config (progn
            (use-package ruby-dev
              :ensure t
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

(use-package python
  :config (progn
            (~auto-load-mode '("\\.py$") 'python-mode)

            ;; Workaround: virtualenvwrapper.el needs to be loaded explicitly
            (progn
              (unless (package-installed-p 'virtualenvwrapper)
                (package-install 'virtualenvwrapper))
              (load-file (~get-library-full-path "virtualenvwrapper")))

            (use-package virtualenvwrapper
              :config (progn
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell)
                        (setq venv-location (or (getenv "WORKON_HOME")
                                                "/m/virtual-envs/"))))

            ;; Doesn't work
            ;; (defun electric-indent-ignore-python (char)
            ;;   "Ignore electric indentation for python-mode"
            ;;   (if (equal major-mode 'python-mode)
            ;;    'no-indent
            ;;  nil))
            ;; (add-hook 'electric-indent-functions 'electric-indent-ignore-python)

            ;; https://www.emacswiki.org/emacs/IndentingPython
            (add-hook 'python-mode-hook
                      (lambda ()
                        (setq electric-indent-chars (delq ?, electric-indent-chars))
                        (setq electric-indent-inhibit t)
                        (~bind-key-with-prefix "e r" 'python-shell-send-region
                                               :keymap python-mode-map)
                        ;; (~bind-key-with-prefix "." 'my/python-jump-to-definition
                        ;;                     :keymap python-mode-map)
                        ;; (~bind-key-with-prefix "," 'my/python-jump-back
                        ;;                     :keymap python-mode-map)
                        ))))

;;
;; Company mode support in Jedi is weak
;;

;; Use autocomplete mode
;; (use-package jedi
;;   :ensure t
;;   :config (progn
;;             (setq jedi:setup-keys nil)
;;             (setq jedi:tooltip-method nil)
;;             (setq jedi:complete-on-dot t)

;;             (autoload 'jedi:setup "jedi" nil t)
;;             (add-hook 'python-mode-hook 'jedi:setup)

;;             (defvar jedi:goto-stack '())

;;             (defun jedi:jump-to-definition ()
;;               (interactive)
;;               (add-to-list 'jedi:goto-stack
;;                            (list (buffer-name) (point)))
;;               (jedi:goto-definition))

;;             (defun jedi:jump-back ()
;;               (interactive)
;;               (let ((p (pop jedi:goto-stack)))
;;                 (if p (progn
;;                         (switch-to-buffer (nth 0 p))
;;                         (goto-char (nth 1 p))))))

;;             (defun my/python-mode-hook ()
;;               (add-to-list 'company-backends 'company-jedi))

;;             (add-hook 'python-mode-hook 'my/python-mode-hook)

;;             ;; (add-hook 'python-mode-hook
;;             ;;           '(lambda ()
;;             ;;              (local-set-key (kbd "C-.") 'jedi:jump-to-definition)
;;             ;;              (local-set-key (kbd "C-,") 'jedi:jump-back)
;;             ;;              (local-set-key (kbd "C-c d") 'jedi:show-doc)
;;             ;;              (local-set-key (kbd "C-<tab>") 'jedi:complete)))
;;             ))

;; Use company-mode
;; (use-package company-jedi
;;   :ensure t
;;   :config (progn
;;             (defvar jedi:goto-stack '())

;;             (defun jedi:jump-to-definition ()
;;               (interactive)
;;               (add-to-list 'jedi:goto-stack
;;                            (list (buffer-name) (point)))
;;               (jedi:goto-definition))

;;             (defun jedi:jump-back ()
;;               (interactive)
;;               (let ((p (pop jedi:goto-stack)))
;;                 (if p (progn
;;                         (switch-to-buffer (nth 0 p))
;;                         (goto-char (nth 1 p))))))

;;             (defun my/python-mode-hook ()
;;               (add-to-list 'company-backends 'company-jedi))

;;             (add-hook 'python-mode-hook 'my/python-mode-hook)))

;;
;; Elpy seems to be a very strong contender
;;
;; http://elpy.readthedocs.io/en/latest/index.html
;; Config with (elpy-config)
;;
;; Setup workflow:
;; * Open a file in the project
;; * Run (pyvenv-workon) and choose the appropriate virtual env
;; * Run (elpy-config) and install necessary dependencies
;;
;; Beginning to work:
;; * Run (pyvenv-workon)
;; * Have fun
;;

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :ensure elpy
  :init (progn
          (elpy-enable)

          ;; I don't want to highlight indentation
          (setq elpy-modules (remove 'elpy-module-highlight-indentation
                                     elpy-modules))

          (setq elpy-rpc-backend "jedi")

          (defvar my/python-goto-stack (list))

          (defun my/python-jump-to-definition ()
            (interactive)
            (add-to-list 'my/python-goto-stack
                         (list (buffer-name) (point)))
            (elpy-goto-definition))

          (defun my/python-jump-back ()
            (interactive)
            (let ((p (pop my/python-goto-stack)))
              (if p (progn
                      (switch-to-buffer (nth 0 p))
                      (goto-char (nth 1 p))))))

          (defun my/elpy-mode-hook ()
            (bind-key "C-c ." 'my/python-jump-to-definition)
            (bind-key "C-c ," 'my/python-jump-back))

          (add-hook 'python-mode-hook 'my/elpy-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config (progn
            ;; (~auto-load-mode '("\\.js\\'") 'js2-mode)
            (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

            (~auto-load-mode '("\\.jsx?\\'") 'js2-jsx-mode)
            (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS-related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scss-mode
  :mode "\\.scss\\$"
  :commands scss-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :ensure t
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
  :ensure t
  :commands toml-mode
  :mode "\\.toml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open last session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (~specialized-emacs?)
  (use-package save-visited-files
    :ensure t
    :config (progn
              (turn-on-save-visited-files-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eshell
  :commands eshell
  :init (progn
          (use-package exec-path-from-shell
            :ensure t)

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
;; Erlang Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package alchemist
;;   :ensure alchemist
;;   :init (progn
;;           (~auto-load-mode '("\\.ex$" "\\.exs$") 'alchemist-mode)

;;           ;; Don't ask to save changes before running tests
;;           (setq alchemist-test-ask-about-save nil)

;;           ;; Display compilation output
;;           (setq alchemist-test-display-compilation-output t)))

(use-package elixir-mode
  :ensure t)

;; (use-package erlang-start
;;   :ensure erlang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package slime
  :ensure t
  :commands common-lisp-mode
  :init
  (progn
    ;; Better indentation, see http://www.emacswiki.org/emacs/IndentingLisp
    (put 'if 'common-lisp-indent-function 2)
    (put 'define-command 'common-lisp-indent-function 2)
    (put 'if-let 'common-lisp-indent-function 2)
    (put 'defcmd 'common-lisp-indent-function 2)
    (put 'define-test 'common-lisp-indent-function 1))
  :config
  (progn
    (defun ~slime-connect-default ()
      "Connects to default Slime process."
      (interactive)
      (slime-connect "localhost" 4005))

    (defun ~cl/eval-open ()
      "Evaluates current expression with Slime and move to the
next line."
      (interactive)
      (ignore-errors
        (call-interactively '~paredit-up-all)
        (call-interactively 'slime-eval-last-expression)
        (next-line)))

    (put 'if 'common-lisp-indent-function 2)

    (add-hook 'inferior-lisp-mode-hook
              (lambda ()
                (inferior-slime-mode t)
                (~load-paredit-mode)
                (eldoc-mode 1)))

    (font-lock-add-keywords 'lisp-mode
                            '(("->" . font-lock-keyword-face)
                              ("->>" . font-lock-keyword-face)))

    (setq inferior-lisp-program *sbcl-bin-path*
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    ;; https://github.com/anwyn/slime-company
    (with-eval-after-load "company"
      (use-package slime-company
        :ensure t)

      (slime-setup '(slime-fancy slime-company))
      ;; (bind-key "C-n" 'company-select-next company-active-map)
      ;; (bind-key "C-p" 'company-select-previous company-active-map)
      ;; (bind-key "C-d" 'company-show-doc-buffer company-active-map)
      ;; (bind-key "M-." 'company-show-location company-active-map)
      )

    (load (concat *quicklisp-path* "slime-helper.el"))
    (load (concat *quicklisp-path* "clhs-use-local.el") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/dominikh/go-mode.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode-autoloads
  :ensure go-mode
  :mode "\\.go\\'"
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CoffeeScript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :defer t
  :init (progn
          ;; (setq whitespace-action '(auto-cleanup))
          ;; (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
          (custom-set-variables '(coffee-tab-width 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quickly copy stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/zonuexe/emacs-copyit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package copyit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just-work jump-to-definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/jacktasia/dumb-jump

(use-package dumb-jump
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation with ease
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package windmove
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pop in/pop out shell buffer easily
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/kyagi/shell-pop-el

(use-package shell-pop
  :ensure t
  :config (progn
            (custom-set-variables
             '(shell-pop-universal-key "<S-f9>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rc shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rc-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nginx mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nginx-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dockerfile mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode
  :mode "Dockerfile\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mixing tabs and spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/SmartTabs#toc1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-tabs-mode
  :ensure t
  :init (progn
          ;; (smart-tabs-insinuate 'c 'javascript 'python)
          (setq-default tab-width 4)
          ;; (add-hook 'python-mode-hook
          ;;           (lambda ()
          ;;             (setq indent-tabs-mode t)
          ;;             (setq tab-width (default-value 'tab-width))
          ;;             (setq whitespace-style '(trailing empty))))
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg-specific package")
(provide 'ee:config-cmpitg-packages)
