;;
;; Copyright (C) 2014-2016 Ha-Duong Nguyen (@cmpitg)
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
;;
;; Visit anything.gpg and it will encrypt it when you save the buffer.
;;
;; To prevent EPG from prompting for a key every time you save a file, put the
;; following at the top of your file:
;;
;;    -*- epa-file-encrypt-to: ("your@email.address") -*-
;;

(use-package epa-file
  :ensure epa
  :config (progn
            (epa-file-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control systems: Mercurial and Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package monky
  :ensure t
  :commands monky-status)

(use-package magit
  :ensure t
  :commands (magit-status ~get-scm magit-get-top-dir))

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
                                                "/m/virtual-envs/"))))))

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
            (add-hook 'js-mode-hook 'js2-minor-mode)
            ;; (~auto-load-mode '("\\.js\\'") 'js2-mode)
            ;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

            ;; (add-hook 'html-mode-hook '~auto-reload-firefox-after-save-hook)
            ;; (add-hook 'css-mode-hook '~auto-reload-firefox-after-save-hook)
            ))

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
;; ibus bridge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Deprecated.  The new iBus 1.5 is totally broken, rendering this useless
;;

;; (load-file (~get-config "local-packages/ibus.el/ibus-dev.el"))
;; (use-package ibus
;;   :commands ibus-mode
;;   :config (progn
;;             ;; Use C-SPC for Set Mark command
;;             (ibus-define-common-key ?\C-\s nil)
;;             ;; Use C-/ for Undo command
;;             (ibus-define-common-key ?\C-/ nil)
;;             ;; Change cursor color depending on IBus status
;;             (setq ibus-cursor-color '("red" "blue" "limegreen"))
;;             (setq ibus-agent-file-name
;;                   (~get-config "local-packages/ibus.el/ibus-el-agent"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open last session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package save-visited-files
  :ensure t
  :config (progn
            (turn-on-save-visited-files-mode)))

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
;; CoffeeScript mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :init (progn
          (setq whitespace-action '(auto-cleanup))
          (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
          (custom-set-variables '(coffee-tab-width 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quickly copy stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/zonuexe/emacs-copyit

(use-package copyit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/syohex/emacs-emamux

(use-package emamux
  :ensure t
  :commands (emamux:send-command emamux:send-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vimdiff implementation - More intuitive than Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/justbur/emacs-vdiff

(use-package vdiff
  :ensure t
  :commands vdiff-files)

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
;; Evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :config (progn
            (use-package evil-paredit
              :ensure t
              :config (progn
                        (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
                        (add-hook 'lisp-mode-hook 'evil-paredit-mode)
                        (add-hook 'scheme-mode-hook 'evil-paredit-mode)))

            (use-package evil-surround
              :ensure t
              :config (progn
                        (global-evil-surround-mode 1)))

            (evil-mode 1)
            (bind-key [mouse-2] nil evil-normal-state-map)
            (bind-key [mouse-2] nil evil-visual-state-map)
            (bind-key [mouse-2] nil evil-insert-state-map)

            ;; (define-key evil-motion-state-map [down-mouse-1] 'evil-mouse-drag-region)
            ;; (lookup-key evil-motion-state-map [down-mouse-1])
            ;; (lookup-key evil-normal-state-map [down-mouse-1])
            ;; (lookup-key evil-visual-state-map [down-mouse-1])

            ;; (delete 'multi-term-mode evil-insert-state-modes)
            ;; (delete 'term-mode evil-insert-state-modes)
            (eval-after-load 'evil-vars
              '(progn
                 (evil-set-initial-state 'term-mode 'emacs)
                 (evil-set-initial-state 'multi-term-mode 'emacs)
                 (evil-set-initial-state 'ansi-term-mode 'emacs)
                 (evil-set-initial-state 'magit-log-edit-mode 'emacs)
                 (evil-set-initial-state 'dired-mode 'emacs)
                 (evil-set-initial-state 'nav-mode 'emacs)
                 (evil-set-initial-state 'grep-mode 'emacs)
                 (evil-set-initial-state 'bs-mode 'emacs)
                 (evil-set-initial-state 'ibuffer-mode 'normal)))

            (setq evil-emacs-state-cursor 'bar)
            (setq-default cursor-type 'hbar)
            (setq-default cursor-type 'bar)

            (bind-spacemacs-like-keys)

            (define-key evil-insert-state-map "\C-y" 'yank)
            (define-key evil-insert-state-map "\C-o" '~open-line)
            (define-key evil-insert-state-map "\C-w" 'kill-region)
            (define-key evil-insert-state-map "\C-k"
              (lambda ()
                (interactive)
                (if (member* mode-name '("Emacs-Lisp" "Lisp") :test 'equalp)
                    (call-interactively 'paredit-kill)
                  (call-interactively 'kill-line))))

            ;; * to search forward, # to search backward
            (use-package evil-visualstar
              :ensure t
              :init (progn
                      (global-evil-visualstar-mode)
                      ;; (setq evil-visualstar/persistent nil)
                      ))

            ;; Better jumper with C-i and C-o
            (use-package evil-jumper
              :ensure t
              :init (progn
                      (evil-jumper-mode t)))

            ;; Doesn't work
            ;; (evil-set-initial-state 'lisp-mode 'emacs)
            ;; (evil-set-initial-state 'emacs-lisp-mode 'emacs)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ee:cmpitg-flavored-packages)
