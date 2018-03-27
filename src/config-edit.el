;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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

;;
;; YAML
;;
;; Ref: https://www.emacswiki.org/emacs/YamlMode
;;

(use-package yaml-mode)

;;
;; JSON
;;
;; Ref: https://github.com/joshwnj/json-mode
;;

(use-package json-mode
  :mode "\\.json\\'")

;;
;; TOML
;;
;; Ref: https://github.com/dryman/toml-mode.el
;;

(use-package toml-mode
  :mode "\\.toml\\'")

;;
;; Chruby
;;
;; Ref: https://github.com/plexus/chruby.el
;;

(use-package chruby
  :config
  (let ((ruby-version (format "ruby-%s"
                              (or (getenv "HELLO") "2.4.1"))))
    (chruby ruby-version)))

;;
;; Vimdiff implementation - More intuitive than Ediff
;;
;; https://github.com/justbur/emacs-vdiff
;;

(use-package vdiff
  :config
  (progn
    (with-eval-after-load "evil"
      (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" 'vdiff-next-hunk)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" 'vdiff-previous-hunk)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zc" 'vdiff-close-fold)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zM" 'vdiff-close-all-folds)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zo" 'vdiff-open-fold)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zR" 'vdiff-open-all-folds)
      (evil-define-minor-mode-key 'motion 'vdiff-mode "go" 'vdiff-receive-changes)
      (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" 'vdiff-send-changes))))

;;
;; Markdown
;;
;; Ref: https://jblevins.org/projects/markdown-mode/
;;

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn
    (custom-set-faces
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(markdown-header-face-1
       ((t (:inherit markdown-header-face :height 1.7 :background "#ABCDEF"))))
     '(markdown-header-face-2
       ((t (:inherit markdown-header-face :height 1.5 :background "green"))))
     '(markdown-header-face-3
       ((t (:inherit markdown-header-face :height 1.3)))))
    
    (add-hook 'markdown-mode-hook #'(lambda ()
                                      (interactive)
                                      (font-lock-mode -1)))))

;;
;; GPG interface
;;
;; Ref: https://www.emacswiki.org/emacs/EasyPG
;;
;; Visit anything.gpg and it will encrypt it when you save the buffer.
;;
;; To prevent EPG from prompting for a key every time you save a file, put the
;; following at the top of your file:
;;
;;    -*- epa-file-encrypt-to: ("your@email.address") -*-
;;

(use-package epa-file
  :config (epa-file-enable))

;;
;; Enhanced file management with Dired
;;

(use-package dired+
  :init (progn
          (setq dired-listing-switches "-lahF")
          ;; Reuse current buffer when opening file/dir
          (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired-single)

(use-package dired-details+
  :after (dired-single)
  :config
  (setq dired-listing-switches "-lhFgG --group-directories-first"))

;;
;; Version control systems: Mercurial and Git
;;
;; Ref: https://github.com/ananthakumaran/monky
;; Ref: https://github.com/magit/magit
;;

(use-package monky
  :commands monky-status)

(use-package magit
  :commands (magit-status magit-get-top-dir)
  :init
  (progn
    (setf magit-push-always-verify 'pp)
    ;; (setf git-commit-check-style-conventions nil)
    (setf git-commit-finish-query-functions nil)
    (with-eval-after-load "evil"
      (~bind-key-with-prefix "g s" #'magit-status))))

;;
;; Rc shell mode
;;
;; Ref: https://github.com/mrhmouse/rc-mode.el
;;

(use-package rc-mode
  :commands (rc-mode))

;;
;; Dockerfile mode
;;
;; Ref: https://github.com/spotify/dockerfile-mode
;;

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;;
;; Nginx mode
;;
;; Ref: https://github.com/ajc/nginx-mode
;;

(use-package nginx-mode
  :mode ("sites-\\(?:available\\|enabled\\)/" . nginx-mode))

;;
;; Mixing tabs and spaces
;;
;; Ref: http://www.emacswiki.org/emacs/SmartTabs
;;

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

;;
;; Rust
;;
;; https://github.com/rust-lang/rust-mode
;;

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode))

;;
;; Golang
;;
;; Ref: https://github.com/dominikh/go-mode.el
;;

(use-package go-mode-autoloads
  :mode "\\.go\\'")

;;
;; CoffeeScript mode
;;
;; Ref: https://github.com/defunkt/coffee-mode
;;

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :init
  (custom-set-variables '(coffee-tab-width 2)))

;;
;; Common Lisp development
;;
;; Ref: https://github.com/slime/slime
;;

(use-package slime
  :commands common-lisp-mode
  :config
  (progn
    (defun ~slime-connect-default ()
      "Connects to default Slime process."
      (interactive)
      (slime-connect "localhost" 4005))

    ;; Better indentation, see http://www.emacswiki.org/emacs/IndentingLisp
    (put 'define-command 'common-lisp-indent-function 2)
    (put 'if-let 'common-lisp-indent-function 2)
    (put 'defcmd 'common-lisp-indent-function 2)
    (put 'define-test 'common-lisp-indent-function 1)
    (put 'if 'common-lisp-indent-function 2)

    (font-lock-add-keywords 'lisp-mode
                            '(("->" . font-lock-keyword-face)
                              ("->>" . font-lock-keyword-face)))

    (setq inferior-lisp-program *sbcl-bin-path*
          slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    (~load-files (concat *quicklisp-path* "slime-helper")
                 (concat *quicklisp-path* "clhs-use-local"))))

;; Ref: https://github.com/anwyn/slime-company
(use-package slime-company
  :after (slime company)
  :config
  (progn
    (slime-setup '(slime-fancy slime-company))
    (bind-key "C-n" 'company-select-next company-active-map)
    (bind-key "C-p" 'company-select-previous company-active-map)
    (bind-key "C-d" 'company-show-doc-buffer company-active-map)
    (bind-key "M-." 'company-show-location company-active-map)))

;;
;; Erlang & Elixir
;;

;; Ref: https://github.com/joaotavora/sly
(use-package alchemist
  :mode (("\\.ex$"  . alchemist-mode)
         ("\\.exs$" . alchemist-mode))
  :init
  (progn
    ;; Don't ask to save changes before running tests
    (setq alchemist-test-ask-about-save nil)

    ;; Display compilation output
    (setq alchemist-test-display-compilation-output t)))

;; Ref: https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode)

;; Ref: http://erlang.org/doc/apps/tools/erlang_mode_chapter.html
(use-package erlang-start)

;; Ref: https://github.com/tjarvstrand/edts
;; edts-man-setup to setup documentation
;; Make sure you have `rebar' 2.0.1+ installed, then
;;   cd ~/.emacs.d/elpa/edts*/
;;   find . -name rebar -print0 | xargs -0 -I{} echo "{}"
;;   make
(use-package edts-start
  :disabled t
  :config (progn
            (setq erlang-electric-commands '(erlang-electric-comma
                                             erlang-electric-semicolon))))

;;
;; Haskell development
;;
;; Ref: https://github.com/haskell/haskell-mode
;;

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config (progn
            (use-package inf-haskell)
            ;; (use-package hs-lint)
            ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
            ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))

;;
;; Julia development
;;
;; Ref: https://github.com/JuliaEditorSupport/julia-emacs
;;

(use-package julia-mode
  :commands julia-mode)

;;
;; CSS-related
;;
;; Ref: https://github.com/antonj/scss-mode
;;

(use-package scss-mode
  :mode "\\.scss\\$")

;;
;; Delphi and Pascal development
;;

(use-package delphi-mode
  :mode (("\\.pas$" . delphi-mode)
         ("\\.pp$"  . delphi-mode)))

;;
;; JavaScript development
;;
;; Ref: https://github.com/mooz/js2-mode
;;

(use-package js2-mode
  :mode (("\\.js\\'"   . js2-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :interpreter "node")

;;
;; Ruby development
;;
;; Ref: https://github.com/Mon-Ouie/ruby-dev.el
;;

(use-package ruby-mode
  :commands ruby-mode
  :mode (("\\Rakefile$" . ruby-mode)
         ("\\.mab$"     . ruby-mode))
  :config (progn
            (use-package ruby-dev
              :init (progn
                      (autoload 'turn-on-ruby-dev "ruby-dev" nil t)
                      (add-hook 'ruby-mode-hook 'turn-on-ruby-dev)))))

;;
;; Python development
;;

(use-package python
  :mode ("\\.py$" . python-mode)
  :config
  (progn
    ;; Ref: https://www.emacswiki.org/emacs/IndentingPython
    (add-hook 'python-mode-hook
              #'(lambda ()
                  (setq electric-indent-chars (delq ?, electric-indent-chars))
                  (setq electric-indent-inhibit t)))))

;;
;; Ref: https://github.com/necaris/conda.el
;; Environment (de)activation: conda-env-activate, conda-env-deactivate
;;

(use-package conda
  :config
  (progn
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)
    (conda-env-autoactivate-mode -1)
    (setq conda-anaconda-home *conda-home-path*)))

;;
;; Ref: http://elpy.readthedocs.io/en/latest/index.html
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

(use-package elpy
  :after conda
  :init (progn
          (elpy-enable)

          ;; I don't want to highlight indentation
          (setq elpy-modules (remove 'elpy-module-highlight-indentation
                                     elpy-modules))

          ;; Completion back-end
          (setq elpy-rpc-backend "jedi")

          (defvar *~python-goto-stack* (list))
          (defun ~python-jump-to-definition ()
            (interactive)
            (add-to-list '*~python-goto-stack*
                         (list (buffer-name) (point)))
            (elpy-goto-definition))
          (defun ~python-jump-back ()
            (interactive)
            (let ((p (pop *~python-goto-stack*)))
              (when p
                (switch-to-buffer (nth 0 p))
                (goto-char (nth 1 p)))))
          
          (setenv "WORKON_HOME" (concat *conda-home-path* "/envs"))

          ;; (~bind-key-with-prefix "d w"   #'pyvenv-workon                     :keymap elpy-mode-map)
          (~bind-key-with-prefix "d w"   #'conda-env-activate                :keymap elpy-mode-map)
          (~bind-key-with-prefix "d z"   #'elpy-shell-switch-to-shell        :keymap elpy-mode-map)
          (~bind-key-with-prefix "d e r" #'elpy-shell-send-region-or-buffer  :keymap elpy-mode-map)
          (~bind-key-with-prefix "d e e" #'elpy-shell-send-current-statement :keymap elpy-mode-map)
          (~bind-key-with-prefix "d ."   #'~python-jump-to-definition        :keymap elpy-mode-map)
          (~bind-key-with-prefix "d ,"   #'~python-jump-back                 :keymap elpy-mode-map)))

;;
;; Clojure development
;;
;;
;; Ref: https://cider.readthedocs.io/en/latest/
;; Ref: https://github.com/clojure-emacs/cider/blob/master/doc/configuration.md
;;

(use-package clojure-mode
  :mode "\\.clj\\'")

(use-package cider
  :after clojure-mode
  :init
  (progn
    (require 'seq)
    ;; Workaround
    (unless (fboundp 'seq-map-indexed)
      (defun seq-map-indexed (function sequence)
        "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of
the sequence, and its index within the sequence."
        (let ((index 0))
          (seq-map (lambda (elt)
                     (prog1
                         (funcall function elt index)
                       (setq index (1+ index))))
                   sequence)))))
  :config
  (progn
    ;; (add-hook 'clojure-mode-hook #'cider-mode)
    (add-hook 'clojure-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'midje-mode)

    ;; Only display eldoc for current function/macro, not current symbol
    (setq cider-eldoc-display-for-symbol-at-point nil)
    (add-hook 'cider-mode-hook 'eldoc-mode)

    (add-hook 'cider-repl-mode-hook #'paredit-mode)

    ;; Moving inside subword
    (add-hook 'cider-repl-mode-hook #'subword-mode)

    ;; Hide *nrepl-connection* and *nrepl-server*
    (setq nrepl-hide-special-buffers t)

    ;; Prevent the auto-display of the REPL buffer in a separate window
    ;; after connection is established
    ;; (setq cider-repl-pop-to-buffer-on-connect nil)
    (setq cider-repl-pop-to-buffer-on-connect t)

    (setq cider-popup-stacktraces nil)

    ;; Enable error buffer popping also in the REPL
    (setq cider-repl-popup-stacktraces t)

    ;; Default value: "repl -s -H :: wait"
    (setq cider-boot-parameters "cider repl -s wait")

    (setq nrepl-buffer-name-separator "-")
    (setq nrepl-buffer-name-show-port t)

    (setq cider-repl-history-size 9999)

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

    (~bind-key-with-prefix "d z"   #'cider-switch-to-repl-buffer :keymap cider-mode-map)
    (~bind-key-with-prefix "d a d" #'~clojure/add-dependency     :keymap cider-mode-map)
    (~bind-key-with-prefix "d ."   #'cider-find-var              :keymap cider-mode-map)
    (~bind-key-with-prefix "d l a" #'cider-load-all-files        :keymap cider-mode-map)
    (~bind-key-with-prefix "d c"   #'cider-repl-clear-buffer     :keymap cider-repl-mode-map)
    (~bind-key-with-prefix "d ."   #'cider-find-var              :keymap cider-repl-mode-map)
    (~bind-key-with-prefix "d l a" #'cider-load-all-files        :keymap cider-repl-mode-map)
    (bind-key "<C-return>" #'cider-eval-last-sexp cider-mode-map)
    (bind-key "<S-return>" #'cider-eval-sexp-at-point cider-mode-map)))

(use-package clojure-cheatsheet
  :after cider)

(use-package clojurescript-mode)

(use-package midje-mode
  :diminish midje-mode
  :after cider)

;; Clojure docs lookup
(use-package cider-grimoire
  :after cider)

(use-package clj-refactor
  :after cider
  :config
  (progn
    (defalias '~clojure/add-dependency 'cljr-add-project-dependency)
    (defalias '~clojure/add-require 'cljr-add-require-to-ns)

    (defun ~hook/clojure-refactor-mode ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1))

    (add-hook 'clojure-mode-hook #'~hook/clojure-refactor-mode)))

;;
;; HTTP request library
;;
;; Ref: https://github.com/tkf/emacs-request
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
  :commands request)

;;
;; Keybindings
;;

(bind-key "s-v" #'package-list-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring Rmacs for code & text editting")

(provide 'rmacs:config-edit)
