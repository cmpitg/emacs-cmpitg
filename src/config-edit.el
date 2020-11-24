;;  -*- lexical-binding: t; -*-

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

;;
;; Interactive menu
;;
;; Ref: https://github.com/abo-abo/hydra
;;

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

;; TODO: Reevaluate
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
;; Asciidoc mode
;;
;; Ref: https://github.com/sensorflo/adoc-mode
;;

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode)
  :after (hydra)
  :config
  (progn
    (defun ~asciidoc/render (html-path)
      "Renders current file with AsciiDoctor in HTML format."
      (interactive "FHTML output path: ")
      (let ((cmd (format "asciidoctor --out-file %s %s"
                         (shell-quote-argument html-path)
                         (shell-quote-argument (~current-file-full-path)))))
        (~run-process cmd)
        (message "'%s' has finished running" cmd)
        (message "Check %s for output" html-path)))

    (defun ~asciidoc/preview ()
      "Renders and previews current AsciiDoc file in HTML
format."
      (interactive)
      (let ((html-path (~asciidoc/current-temporary-html-path)))
        (~asciidoc/render html-path)
        (~firefox html-path :new-window? t)))

    (defun ~asciidoc/current-temporary-html-path ()
      "Returns the HTML path corresponding to the current
AsciiDoc buffer.  The path is stored in a buffer local variable
named `asciidoc-html-path' and generated if not yet exists"
      (let ((asciidoc-html-path/symbol (make-local-variable 'asciidoc-html-path)))
        (unless (boundp asciidoc-html-path/symbol)
          (set asciidoc-html-path/symbol (make-temp-file (f-filename (buffer-file-name))
                                                         nil
                                                         ".html")))
        (buffer-local-value 'asciidoc-html-path (current-buffer))))

    (defun ~asciidoc/update-preview ()
      "Re-renders current AsciiDoc file for preview.  The browser
might need manual refreshing."
      (interactive)
      (~asciidoc/render (~asciidoc/current-temporary-html-path)))

    (defhydra hydra-asciidoc (:exit t)
      "Asciidoc operations"
      ("r" #'~asciidoc/render "Render")
      ("p" #'~asciidoc/preview "Preview")
      ("u" #'~asciidoc/update-preview "Update preview"))))

;;
;; Just-work jump-to-definition
;;
;; Ref: https://github.com/jacktasia/dumb-jump
;;

(use-package dumb-jump
  :config
  (progn
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-prefer-searcher 'rg)))

(use-package smart-jump
  :ensure t
  :config (smart-jump-setup-default-registers))

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
  :mode ("\\.rb\\'" "Rakefile")
  :defer t
  :config
  (let ((ruby-version (or (getenv "CHRUBY_VERSION") "ruby-2.5.1")))
    (chruby ruby-version)))

;;
;; Vimdiff implementation - More intuitive than Ediff
;;
;; https://github.com/justbur/emacs-vdiff
;;

(use-package vdiff
  :commands (vdiff-mode vdiff-files vdiff-files3)
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

(require 'epa-file)
(epa-file-enable)

;;
;; Enhanced file management with Dired
;;

(use-package dired+
  :init (progn
          ;; (setq dired-listing-switches "-lahF")
          ;; Reuse current buffer when opening file/dir
          (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired-single)

(use-package dired-details+
  :after (dired-single)
  :config
  (setq dired-listing-switches "-lahFgG --group-directories-first"))

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
          (setq-default tab-width 4)))

;;
;; Rust
;;
;; https://github.com/rust-lang/rust-mode
;;

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))

;;
;; Golang
;;
;; Ref: https://github.com/dominikh/go-mode.el
;;

(use-package go-mode
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
  :mode "\\.lua\\'"
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

(remove-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
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
  :mode (("\\.ex\\'"  . alchemist-mode)
         ("\\.exs\\'" . alchemist-mode))
  :init
  (progn
    ;; Don't ask to save changes before running tests
    (setq alchemist-test-ask-about-save nil)

    ;; Display compilation output
    (setq alchemist-test-display-compilation-output t)))

;; Ref: https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode
  :defer t
  :mode ("\\.ex\\'"
         "\\.exs\\'"))

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
            (require 'inf-haskell)
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
  :mode "\\.jl\\'"
  :commands julia-mode)

;;
;; CSS-related
;;
;; Ref: https://github.com/antonj/scss-mode
;;

(use-package scss-mode
  :mode "\\.scss\\'")

;;
;; Delphi and Pascal development
;;
;; Ref: https://github.com/ki11men0w/emacs-delphi-mode
;;

(use-package opascal-mode
  :straight
  (opascal-mode :type git :host github :repo "ki11men0w/emacs-delphi-mode")
  :mode (("\\.pas\\'" . opascal-mode)
         ("\\.pp\\'"  . opascal-mode)))

;;
;; JavaScript development
;;
;; Ref: https://github.com/mooz/js2-mode
;; Ref: https://github.com/ananthakumaran/tide
;;

(use-package js2-mode
  :mode (("\\.js\\'"   . js2-minor-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :interpreter "node")

(use-package add-node-modules-path
  :after (js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook #'add-node-modules-path)
    (add-hook 'js2-jsx-mode-hook #'add-node-modules-path)))

(use-package tide
  :after (flycheck company)
  :mode (("\\.tsx\\'" . typescript-mode))
  :config
  (progn
    (with-eval-after-load "dtrt-indent"
      (add-to-list 'dtrt-indent-hook-mapping-list
                   '(typescript-mode javascript typescript-indent-level)))

    (defun my/setup-javascript-dev ()
      (interactive)
      (tide-setup)
      (eldoc-mode 1)
      (dtrt-indent-mode 1)
      (flycheck-mode 1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (tide-hl-identifier-mode 1))

    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)

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
  :mode (("\\Rakefile\\'" . ruby-mode)
         ("\\.mab\\'"     . ruby-mode))
  :config (progn
            (use-package robe
              :init (progn
                      (add-hook 'ruby-mode-hook 'robe-mode)
                      (with-eval-after-load "company-mode"
                        (push 'company-robe company-backends))))))

;;
;; Python development
;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
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

          (setq python-shell-interpreter "ipython")
          (setq python-shell-interpreter-args "-i --simple-prompt")
          (setq elpy-rpc-python-command "python3")
          (setq elpy-rpc-virtualenv-path 'default)

          (defhydra hydra-dev-python-conda (:columns 4 :exit t)
            "Python development with Conda"
            ("a" #'conda-env-activate "Activate virtualenv"))

          (defhydra hydra-dev-python (:columns 4 :exit t)
            "Python development"
            ("a" #'pyvenv-activate "Activate virtualenv")
            ("z" #'elpy-shell-switch-to-shell "Switch to shell")
            ("r" #'elpy-shell-send-region-or-buffer "Eval region or buffer")
            ("e" #'elpy-shell-send-current-statement "Eval current statement")
            ("f" #'elpy-format-code "Format code")
            ("." #'~python-jump-to-definition "Jump to definition")
            ("," #'~python-jump-back "Jump back" :exit nil)
            ("l" #'elpy-config "Config")
            ("c" #'hydra-dev-python-conda/body "Conda"))))

;;
;; Clojure development
;;
;;
;; Ref: https://cider.readthedocs.io/en/latest/
;; Ref: https://github.com/clojure-emacs/cider/blob/master/doc/configuration.md
;;

(use-package clojure-mode
  :after paredit-mode
  :mode "\\.clj\\'"
  :init
  (progn
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
      (are '(2 1))
      (keep-focused 1))))

(use-package flycheck-clj-kondo
  :after clojure-mode
  :init
  (progn
    (defun my/enable-clj-syntax-check ()
      "Enables syntax check for Clojure."
      (interactive)
      (flycheck-mode 1))

    (add-hook 'clojure-mode-hook #'my/enable-clj-syntax-check)))

(use-package cider
  :after (hydra clojure-mode)
  :hook (((clojure-mode) . midje-mode)
         ((cider-repl-mode) . subword-mode)
         ((clojure-mode
           cider-mode) . eldoc-mode))
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

    (defun ~cider-format-defun ()
      "Interactively calls `cider-connect', saving the current cursor position."
      (interactive)
      (save-excursion
        (call-interactively 'cider-format-defun)))

    ;; Only display eldoc for current function/macro, not current symbol
    (setq cider-eldoc-display-for-symbol-at-point nil)

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

    ;; Do not pop up REPL after connecting
    (setq cider-repl-pop-to-buffer-on-connect nil)

    (defhydra hydra-dev-clojure (:columns 4 :exit t)
      "Clojure development"
      ("dpa" #'~clojure/add-dependency "Dependency: Add")
      ("ddd" #'cider-doc "Doc: Show")
      ("dda" #'cider-apropos "Doc: Apropos")
      ("nse" #'cider-eval-ns-form "Namespace: Eval ns form")
      ("nsb" #'cider-browse-ns "Namespace: Browse")
      ("ns." #'cider-find-ns "Namespace: Jump to")
      ("nsr" #'cljr-add-require-to-ns "Namespace: Add require")
      ("bl" #'cider-load-buffer "Buffer: Load")
      ("fl" #'cider-load-file "File: Load")
      ("fa" #'cider-load-all-files "File: Load all")
      ("re" #'cider-eval-region "Region: Eval")
      ("ls" #'cider-switch-to-repl-buffer "REPL: Switch to")
      ("lc" #'cider-repl-clear-buffer "REPL: Clear")
      ("ln" #'cider-repl-set-ns "REPL: Set ns")
      ("a" #'clojure-align "Align")
      ("." #'cider-find-var "Jump to var definition")
      ("," #'cider-pop-back "Jump back")
      ("pp" #'cider-pprint-eval-last-sexp "Eval and pp last sexp")
      ("ee" #'cider-eval-last-sexp "Eval last sexp")
      ("ef" #'cider-eval-defun-at-point "Eval defun")
      ("es" #'cider-eval-sexp-at-point "Eval current sexp")
      ("tf" #'~cider-format-defun "Format defun"))

    (bind-key "<C-return>" #'cider-eval-last-sexp      cider-mode-map)
    (bind-key "<M-return>" #'cider-eval-defun-at-point cider-mode-map)
    (bind-key "<S-return>" #'cider-eval-sexp-at-point  cider-mode-map)
    (bind-key "M-q"        #'~cider-format-defun       cider-mode-map)))

(use-package midje-mode
  :diminish midje-mode
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
;; Scheme development
;;
;; Ref: https://www.nongnu.org/geiser/
;;

(use-package guix-devel
  :straight
  (guix :type git :host github :repo "alezost/guix.el")
  :mode "\\.scm\\'")

(use-package geiser
  :mode "\\.scm\\'"
  :config
  (progn
    (setq geiser-repl-use-other-window nil)
    (setq geiser-default-implementation 'guile)))

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
    (cl-defun ~tcl/browse-doc (&optional (version *~tcl-version*))
      "Browses Tcl documentation."
      (interactive)
      (w3m (format "https://www.tcl.tk/man/tcl%s/" version)))

    (defhydra hydra-dev-tcl (:columns 4 :exit t)
      "Python development"
      ("z" #'switch-to-tcl "Switch to shell")
      ("r" #'tcl-eval-region "Eval region"))))

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

;; (use-package request
;;   :disabled t
;;   :commands request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Rmacs for code & text editting")

(provide 'rmacs:config-edit)
