;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2014-2019 Ha-Duong Nguyen (@cmpitg)
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
;; Interactive menu
;;
;; Ref: https://github.com/abo-abo/hydra
;;

(use-package hydra)

(use-package ivy-hydra
  :after (ivy counsel))

;;
;; Sublime-like C-p
;; Ref: https://github.com/vspinu/imenu-anywhere
;;

(use-package imenu-anywhere
  :after (counsel))

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
;; Pairs management
;;
;; Ref: https://github.com/cute-jumper/embrace.el
;;

(use-package embrace)

;;
;; Just-work jump-to-definition
;;
;; Ref: https://github.com/jacktasia/dumb-jump
;;

(use-package dumb-jump
  :config
  (progn
    (setq dumb-jump-selector 'ivy)))

;;
;; Bracket-based structured editing
;;
;; Ref: https://www.emacswiki.org/emacs/ParEdit
;;

(use-package paredit
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          clojure-mode) . paredit-mode)
  :bind (:map paredit-mode-map
         ("s-." . paredit-backward-kill-word)
         ("s-p" . paredit-forward-kill-word)
         ("s-r" . forward-word)
         ("s-g" . backward-word)
         ("s-C" . paredit-backward-up)
         ("s-T" . paredit-forward-up)
         ("s-R" . paredit-forward)
         ("s-G" . paredit-backward))
  :defines slime-repl-mode-map
  :config (progn
            ;; Always try to delete region first
            (put 'paredit-forward-delete 'delete-selection 'supersede)
            (put 'paredit-backward-delete 'delete-selection 'supersede)

            (defun ~enable-paredit-mode ()
              "Enables paredit mode"
              (interactive)
              (paredit-mode 1))

            (defun ~advice/disable-other-parens-modes-in-paredit (orig-fun &rest args)
              (when (apply orig-fun args)
                (when (fboundp 'autopair-mode)
                  (autopair-mode -1))
                (when (fboundp 'smartparens-mode)
                  (smartparens-mode -1))))
            (advice-add 'paredit-mode
                        :around #'~advice/disable-other-parens-modes-in-paredit)

            ;; Use in minibuffer
            (defun conditionally-enable-paredit-mode ()
              "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
              (if (eq this-command 'eval-expression)
                  (paredit-mode 1)))
            (add-hook 'minibuffer-setup-hook #'conditionally-enable-paredit-mode)

            ;; Use with SLIME REPL
            (add-hook 'slime-repl-mode-hook #'~enable-paredit-mode)

            ;; Stop SLIME's REPL from grabbing DEL,
            ;; which is annoying when backspacing over a '('
            (defun override-slime-repl-bindings-with-paredit ()
              (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
            (add-hook 'slime-repl-mode-hook
                      #'override-slime-repl-bindings-with-paredit)

            (defun ~paredit-up-all ()
              (interactive)
              (ignore-errors
                (loop do (paredit-forward-up))))))

(use-package evil-paredit
  :after (evil paredil)
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          scheme-mode) . evil-paredit-mode))

;;
;; Tmux interaction
;;
;; https://github.com/syohex/emacs-emamux
;;

(use-package emamux
  :commands (emamux:send-command emamux:send-region))

;;
;; File explorer and sidebar
;;
;; Ref: https://github.com/Alexander-Miller/treemacs
;;

(use-package treemacs
  :after (evil)
  :disabled t
  :config
  (progn
    (treemacs-follow-mode -1)
    (treemacs-filewatch-mode -1)
    ;; Collapse empty dirs into one when possible
    (setq treemacs-collapse-dirs 3)
    ;; Always find and focus on the current file when treemacs is built
    (setq treemacs-follow-after-init t)))
(use-package treemacs-evil
  :after (treemacs evil)
  :disabled t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :disabled t
  :config (setq treemacs-header-function #'treemacs-projectile-create-header))

;;
;; Chruby
;;
;; Ref: https://github.com/plexus/chruby.el
;;

(use-package chruby
  :config
  (let ((ruby-version (or (getenv "CHRUBY_VERSION") "ruby-2.5.1")))
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
;; Lua mode
;;
;; Ref: https://github.com/immerrr/lua-mode
;;

(use-package lua-mode
  :config
  (progn
    ;; Freaking stupid indentation rule...
    (setq lua-indent-level 2)))

;;
;; Common Lisp development
;;

;;
;; Ref: https://github.com/joaotavora/sly
;;

(use-package sly
  :commands common-lisp-mode
  :config
  (progn
    (setq inferior-lisp-program *sbcl-bin-path*)

    (bind-key "<C-return>" #'sly-eval-last-expression  sly-mode-map)
    (bind-key "<M-return>" #'sly-eval-defun            sly-mode-map)))

;;
;; Ref: https://github.com/slime/slime
;;

(use-package slime
  :commands common-lisp-mode
  :disabled t
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
  :disabled t
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

;; Ref: https://github.com/tonini/alchemist.el
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

(use-package tide
  :after (flycheck company)
  :config
  (progn
    (defun my/setup-javascript-dev ()
      (interactive)
      (tide-setup)
      (eldoc-mode 1)
      (flycheck-mode 1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (tide-hl-identifier-mode 1))

    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

    (add-hook 'js2-mode-hook #'my/setup-javascript-dev)
    (add-hook 'js2-jsx-mode-hook #'my/setup-javascript-dev)
    (add-hook 'typescript-mode-hook #'my/setup-javascript-dev)))

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
  :after (exec-path-from-shell)
  :config
  (progn
    (conda-env-initialize-interactive-shells)
    (conda-env-initialize-eshell)
    (conda-env-autoactivate-mode -1)
    (setq conda-anaconda-home *conda-home-path*)

    (let ((conda-bin (concat *conda-home-path* "/bin"))
          (current-env-path (getenv "PATH")))
      (unless (string-prefix-p conda-bin current-env-path)
        (setenv "PATH" (concat conda-bin ":" current-env-path))
        (exec-path-from-shell-copy-env "PATH")))))

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
          (setq elpy-rpc-python-command (concat *conda-home-path* "/bin/python"))

          (setq python-shell-interpreter "ipython")
          (setq python-shell-interpreter-args "-i --simple-prompt")

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

(use-package flycheck-clj-kondo
  :init
  (progn
    (defun my/enable-clj-syntax-check ()
      "Enables syntax check for Clojure."
      (interactive)
      (flycheck-mode 1))

    (add-hook 'clojure-mode-hook #'my/enable-clj-syntax-check)))

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
    (defun ~cider-connect ()
      "Interactively calls `cider-connect', saving the current cursor position."
      (interactive)
      (save-excursion
        (call-interactively 'cider-connect)))

    ;; (add-hook 'clojure-mode-hook #'cider-mode)
    (add-hook 'clojure-mode-hook #'~enable-paredit-mode)
    (add-hook 'clojure-mode-hook #'midje-mode)

    ;; Only display eldoc for current function/macro, not current symbol
    (setq cider-eldoc-display-for-symbol-at-point nil)
    (add-hook 'cider-mode-hook 'eldoc-mode)

    (add-hook 'cider-repl-mode-hook #'~enable-paredit-mode)

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

    ;; Do not pop up REPL after connecting
    (setq cider-repl-pop-to-buffer-on-connect nil)

    (~bind-key-with-prefix "d d e a"   #'~clojure/add-dependency     :keymap cider-mode-map)
    (~bind-key-with-prefix "d d d"     #'cider-doc                   :keymap cider-mode-map)
    (~bind-key-with-prefix "d d w"     #'cider-grimoire-web          :keymap cider-mode-map)
    (~bind-key-with-prefix "d d g"     #'cider-grimoire              :keymap cider-mode-map)
    (~bind-key-with-prefix "d d a"     #'cider-apropos               :keymap cider-mode-map)
    (~bind-key-with-prefix "d n s e"   #'cider-eval-ns-form          :keymap cider-mode-map)
    (~bind-key-with-prefix "d n s b"   #'cider-browse-ns             :keymap cider-mode-map)
    (~bind-key-with-prefix "d n s v"   #'cider-find-ns               :keymap cider-mode-map)
    (~bind-key-with-prefix "d n s a r" #'cljr-add-require-to-ns      :keymap cider-mode-map)
    (~bind-key-with-prefix "d s r"     #'cider-switch-to-repl-buffer :keymap cider-mode-map)
    (~bind-key-with-prefix "d b l"     #'cider-load-buffer           :keymap cider-mode-map)
    (~bind-key-with-prefix "d f l"     #'cider-load-file             :keymap cider-mode-map)
    (~bind-key-with-prefix "d f a"     #'cider-load-all-files        :keymap cider-mode-map)
    (~bind-key-with-prefix "d a a"     #'clojure-align               :keymap cider-mode-map)
    (~bind-key-with-prefix "d p p"     #'cider-pprint-eval-last-sexp :keymap cider-mode-map)
    (~bind-key-with-prefix "d ."       #'cider-find-var              :keymap cider-mode-map)
    (~bind-key-with-prefix "d ,"       #'cider-pop-back              :keymap cider-mode-map)

    (~bind-key-with-prefix "d l"     #'cider-repl-clear-buffer :keymap cider-repl-mode-map)
    (~bind-key-with-prefix "d ."     #'cider-find-var          :keymap cider-repl-mode-map)
    (~bind-key-with-prefix "d ,"     #'cider-pop-back          :keymap cider-repl-mode-map)
    (~bind-key-with-prefix "d f a"   #'cider-load-all-files    :keymap cider-repl-mode-map)
    (~bind-key-with-prefix "d n s s" #'cider-repl-set-ns       :keymap cider-repl-mode-map)

    (bind-key "<C-return>" #'cider-eval-last-sexp      cider-mode-map)
    (bind-key "<M-return>" #'cider-eval-defun-at-point cider-mode-map)
    (bind-key "<S-return>" #'cider-eval-sexp-at-point  cider-mode-map)))

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
;; Zig mode
;;
;; Ref: https://github.com/ziglang/zig-mode
;;

(use-package zig-mode
  :mode (("\\.zig\\'" . zig-mode)))

;;
;; Tcl mode
;;

(use-package tcl
  :init
  (progn
    (defvar *~tcl-version* "8.7")
    (defun* ~tcl/browse-doc (&optional (version *~tcl-version*))
      "Browses Tcl documentation."
      (interactive)
      (w3m (format "https://www.tcl.tk/man/tcl%s/" version)))))

;;
;; Language server mode
;;
;; Ref: https://github.com/joaotavora/eglot
;;
;; Supported languages:
;; - C, C++: Clangd
;; - Rust: https://github.com/rust-lang/rls
;;

(use-package eglot
  :config (progn
            (add-to-list 'eglot-server-programs
                         '((c++-mode c-mode) "clangd-6.0"))
            (add-hook 'rust-mode-hook 'eglot-ensure)))

;;
;; Acme-like command palette
;;

;; (require 'rmacs:config-module-command-palette)
;; (setq command-palette:*default-content* "save-cp buffers +tool +frame +bm ojo")
;; (command-palette-mode -1)

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
  :disabled t
  :commands request)

;;
;; Keybindings
;;

(bind-key "s-v" #'package-list-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Rmacs for code & text editting")

(provide 'rmacs:config-edit)
