;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2014-2025 Ha-Duong Nguyen (@cmpitg)
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
;; Treesitter bundle
;;
;; Ref: https://emacs-tree-sitter.github.io/
;;
;; Helpful doc: https://emacs-tree-sitter.github.io/getting-started/
;;

;; To check available languages: tree-sitter-major-mode-language-alist

(use-package tree-sitter
  :ensure t)
(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter)
  :config
  (progn
    (global-tree-sitter-mode 1)

    ;; Replace the default highlight mechanisms with tree-sitter's whenever possible
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

;;
;; YAML
;;
;; Ref: https://github.com/ikatyang/tree-sitter-yaml
;;

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'"
         "\\.yml\\'"))

;;
;; Nix
;;
;; Ref: https://github.com/NixOS/nix-mode
;; Ref: https://github.com/nix-community/nix-ts-mode
;;

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;;
;; Nickel
;;
;; Ref: https://github.com/nickel-lang/nickel-mode
;;

(use-package nickel-mode
  :mode "\\.ncl\\'"
  :ensure t
  :config
  (with-eval-after-load "eglot-mode"
    (add-to-list 'eglot-server-programs '(nickel-mode . ("nls")))
    (add-hook 'nickel-mode-hook 'eglot-ensure)))

;;
;; Dhall
;;
;; Ref: https://github.com/psibi/dhall-mode
;;

(use-package dhall-mode
  :mode "\\.dhall\\'")

;;
;; Caddyfile
;;
;; Ref: https://github.com/Schnouki/caddyfile-mode
;;

(use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("Caddyfile.local\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

;;
;; Just
;;
;; Ref: https://github.com/psibi/justl.el
;; Ref: https://github.com/leon-barrett/just-mode.el
;;

(use-package justl
  :ensure t)

(use-package just-mode
  :ensure t
  :custom
  (tab-width 2))

;;
;; Asciidoc mode
;;
;; Ref: https://github.com/sensorflo/adoc-mode
;; Ref: https://github.com/bbatsov/adoc-mode
;;

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode))
  :bind* (:map
          adoc-mode-map
          ("M-SPC e r" . #'~asciidoc/render)
          ("M-SPC e p" . #'~asciidoc/preview)
          ("M-SPC e u" . #'~asciidoc/update-preview))
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
      (~asciidoc/render (~asciidoc/current-temporary-html-path)))))

;;
;; Just-work jump-to-definition
;;
;; Ref: https://github.com/jacktasia/dumb-jump
;;

(use-package dumb-jump
  :ensure t
  :config
  (progn
    ;; (setq dumb-jump-selector 'popup)
    (setq dumb-jump-prefer-searcher 'rg)))

(use-package smart-jump
  :ensure t
  :config (smart-jump-setup-default-registers))

;;
;; Sidebar with file explorer
;;
;; Ref: https://github.com/jaypei/emacs-neotree
;;

(use-package neotree
  :ensure t
  :commands (neotree-toggle
             neotree-dir))

;;
;; Project-based file explorer sidebar
;;
;; Ref: https://github.com/Alexander-Miller/treemacs
;;

(use-package treemacs
  :ensure t
  :config
  (progn
    (treemacs-follow-mode 1)
    (treemacs-filewatch-mode -1)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    ;; Collapse empty dirs into one when possible
    (setq treemacs-collapse-dirs 3)
    ;; Always find and focus on the current file when treemacs is built
    (setq treemacs-follow-after-init t)

    ;; (treemacs-start-on-boot)
    ))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :after (treemacs)
  :ensure t)
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
(use-package treemacs-persp
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))
(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
(use-package treemacs-evil
  :after (treemacs evil)
  :disabled t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :disabled t
  :config (setq treemacs-header-function #'treemacs-projectile-create-header))

;;
;; Markdown
;;
;; Ref: https://jblevins.org/projects/markdown-mode/
;;

(use-package markdown-mode
  :ensure t
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
;; Ref: https://github.com/emacsattic/dired-single
;;

(use-package dired+
  :ensure (:host github :repo "emacsmirror/dired-plus")
  :config (progn
            ;; (setq dired-listing-switches "-lahF")
            ;; Reuse current buffer when opening file/dir
            (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired-single
  :ensure (:host github :repo "emacsattic/dired-single"))

(use-package dired-details+
  :disabled t
  :after (dired-single))

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
  :ensure t
  :mode ("Dockerfile\\'"
         "Containerfile\\'"))

;;
;; Nginx mode
;;
;; Ref: https://github.com/ajc/nginx-mode
;;

(use-package nginx-mode
  :ensure t
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
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

;;
;; Golang
;;
;; Ref: https://github.com/dominikh/go-mode.el
;;

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

;;
;; Lua mode
;;
;; Ref: https://github.com/immerrr/lua-mode
;;

(use-package lua-mode
  :ensure t
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
  :ensure t
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

;; TODO
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
  :ensure t
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
  :ensure t
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
  :ensure t
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
  :ensure t
  :mode "\\.jl\\'"
  :commands julia-mode)

;;
;; CSS-related
;;
;; Ref: https://github.com/antonj/scss-mode
;;

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'")

;;
;; Delphi and Pascal development
;;
;; Ref: https://github.com/ki11men0w/emacs-delphi-mode
;;

(use-package opascal-mode
  :ensure
  (opascal :host github :repo "ki11men0w/emacs-delphi-mode")
  :mode (("\\.pas\\'" . opascal-mode)
         ("\\.pp\\'"  . opascal-mode)))

;;
;; JavaScript development
;;
;; Ref: https://github.com/mooz/js2-mode
;; Ref: https://github.com/ananthakumaran/tide
;;

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'"   . js2-minor-mode)
         ("\\.jsx?\\'" . js2-jsx-mode))
  :interpreter "node")

(use-package add-node-modules-path
  :disabled t
  :after (js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook #'add-node-modules-path)
    (add-hook 'js2-jsx-mode-hook #'add-node-modules-path)))

(use-package tide
  :ensure t
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
;; Python development
;;

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (progn
    (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
    (setenv "PYTHONIOENCODING" "utf-8")

    ;; Ref: https://www.emacswiki.org/emacs/IndentingPython
    (add-hook 'python-mode-hook
              #'(lambda ()
                  (setq electric-indent-chars (delq ?, electric-indent-chars))
                  (setq electric-indent-inhibit t)))))

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
  :ensure t
  :after (company)
  :bind* (("M-SPC u p a" . #'pyvenv-activate)
          ("M-SPC u p z" . #'elpy-shell-switch-to-shell)
          ("M-SPC u p r" . #'elpy-shell-send-region-or-buffer)
          ("M-SPC u p e" . #'elpy-shell-send-current-statement)
          ("M-SPC u p f" . #'elpy-format-code)
          ("M-SPC u p ." . #'~python-jump-to-definition)
          ("M-SPC u p ," . #'~python-jump-back)
          ("M-SPC u p l" . #'elpy-config)
          ("M-SPC u p c a" . #'conda-env-activate))
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
          (setq elpy-rpc-virtualenv-path 'default)))

;;
;; Clojure development
;;
;;
;; Ref: https://cider.readthedocs.io/en/latest/
;; Ref: https://github.com/clojure-emacs/cider/blob/master/doc/configuration.md
;;

(use-package clojure-mode
  :ensure t
  :mode "\\.clj\\'"
  :config
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
  :ensure t
  :after clojure-mode
  :init
  (progn
    (defun my/enable-clj-syntax-check ()
      "Enables syntax check for Clojure."
      (interactive)
      (flycheck-mode 1))

    (add-hook 'clojure-mode-hook #'my/enable-clj-syntax-check)))

(use-package cider
  :ensure t
  :after (clojure-mode yasnippet)
  :hook (((cider-repl-mode) . subword-mode)
         ((clojure-mode
           cider-mode) . eldoc-mode))
  :bind* (("M-SPC u j d p a" . #'~clojure/add-dependency)
          ("M-SPC u j d d d" . #'cider-doc)
          ("M-SPC u j d d a" . #'cider-apropos)
          ("M-SPC u j n s e" . #'cider-eval-ns-form)
          ("M-SPC u j n s b" . #'cider-browse-ns)
          ("M-SPC u j n s ." . #'cider-find-ns)
          ("M-SPC u j n s r" . #'cljr-add-require-to-ns)
          ("M-SPC u j b l" . #'cider-load-buffer)
          ("M-SPC u j f l" . #'cider-load-file)
          ("M-SPC u j f a" . #'cider-load-all-files)
          ("M-SPC u j r e" . #'cider-eval-region)
          ("M-SPC u j l s" . #'cider-switch-to-repl-buffer)
          ("M-SPC u j l c" . #'cider-repl-clear-buffer)
          ("M-SPC u j l n" . #'cider-repl-set-ns)
          ("M-SPC u j a" . #'clojure-align)
          ("M-SPC u j ." . #'cider-find-var)
          ("M-SPC u j ," . #'cider-pop-back)
          ("M-SPC u j p p" . #'cider-pprint-eval-last-sexp)
          ("M-SPC u j e e" . #'cider-eval-last-sexp)
          ("M-SPC u j e f" . #'cider-eval-defun-at-point)
          ("M-SPC u j e s" . #'cider-eval-sexp-at-point)
          ("M-SPC u j t f" . #'~cider-format-defun))
  :init
  (progn
    (require 'seq)
    ;; Workaround
    (unless (fboundp 'seq-map-indexed)
      (defun seq-map-indexed (function sequence)
        "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of the
sequence, and its index within the sequence."
        (let ((index 0))
          (seq-map (lambda (elt)
                     (prog1
                         (funcall function elt index)
                       (setq index (1+ index))))
                   sequence)))))
  :config
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

  (bind-key "<C-return>" #'cider-eval-last-sexp      cider-mode-map)
  (bind-key "<M-return>" #'cider-eval-defun-at-point cider-mode-map)
  (bind-key "<S-return>" #'cider-eval-sexp-at-point  cider-mode-map)
  (bind-key "M-q"        #'~cider-format-defun       cider-mode-map))

;;
;; Scheme development
;;
;; Ref: https://www.nongnu.org/geiser/
;;

(use-package guix-devel
  :disabled t
  :straight
  (guix :type git :host github :repo "alezost/guix.el")
  :after (geiser)
  :mode ("\\.scm\\'")
  :init
  (progn
    (add-to-list 'safe-local-variable-values
                 '(eval let
                        ((root-dir-unexpanded
                          (locate-dominating-file default-directory ".dir-locals.el")))
                        (when root-dir-unexpanded
                          (let*
                              ((root-dir
                                (expand-file-name root-dir-unexpanded))
                               (root-dir*
                                (directory-file-name root-dir)))
                            (unless
                                (boundp 'geiser-guile-load-path)
                              (defvar geiser-guile-load-path 'nil))
                            (make-local-variable 'geiser-guile-load-path)
                            (require 'cl-lib)
                            (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal)))))
    (add-to-list 'safe-local-variable-values
                 '(eval setq-local guix-directory
                        (locate-dominating-file default-directory ".dir-locals.el")))))

(use-package geiser-guile
  :disabled t)

(use-package geiser-mit
  :disabled t
  :mode ("\\.scm\\'")
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
  :ensure t
  :mode (("\\.zig\\'" . zig-mode)))

;;
;; Tcl mode
;;

(use-package tcl
  :bind* (("M-SPC u t z" . #'switch-to-tcl)
          ("M-SPC u t r" . #'tcl-eval-region))
  :init
  (progn
    (defvar *~tcl-version* "8.7")
    (cl-defun ~tcl/browse-doc (&optional (version *~tcl-version*))
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
(require 'eglot)
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) "clangd-6.0"))
  (add-hook 'rust-mode-hook 'eglot-ensure))

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
