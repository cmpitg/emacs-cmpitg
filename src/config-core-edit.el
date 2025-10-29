;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2018-2025 Ha-Duong Nguyen (@cmpitg)
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

(require 'cl-lib)
(require 'misc)
(require 'thingatpt)
(require 'subr-x)

;;
;; Navigate buffers based on MRU/recency
;;
;; Ref: https://github.com/jrosdahl/iflipb
;;
(use-package iflipb
  :ensure t)

;;
;; String processing
;;
;; Ref: https://github.com/magnars/s.el
;;

(use-package s
  :ensure t)

;;
;; File/filesystem library
;;
;; Ref: https://github.com/rejeep/f.el
;;

(use-package f
  :ensure t)

;;
;; Async processing by spawning subordinate processes
;;
;; Ref: https://github.com/jwiegley/emacs-async
;;

(use-package async
  :ensure t)

;;
;; Editable grep'ing
;;

(use-package wgrep
  :ensure t)

;;
;; Tramp for remote & sudo access
;;

(require 'tramp)
;; Tramp hangs due to fancy shell prompt -
;; https://www.emacswiki.org/emacs/TrampMode#toc12
(custom-set-variables `(tramp-default-method "ssh")
                      `(tramp-terminal-type "dumb")
                      `(tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))

;;
;; Expand selection with one keybinding
;;
;; Ref: https://github.com/magnars/expand-region.el
;;

(use-package expand-region
  :ensure t
  :commands (er/expand-region
             er/mark-outside-pairs
             er/mark-outside-quotes
             er/mark-defun)
  :bind
  ("C-=" . #'er/expand-region))

;;
;; grep command
;;

(require 'grep)
;; (grep-apply-setting 'grep-command "grep --ignore-case --line-number --with-filename -e ")

;; or,
(grep-apply-setting 'grep-command "rg --ignore-case --line-number --with-filename --vimgrep -e ")
(grep-apply-setting 'grep-use-null-device nil)

;;
;; Save minibuffer history across sessions
;;
;; Ref: https://www.emacswiki.org/emacs/SaveHist
;;

(savehist-mode 1)
(setq savehist-file (~get-rmacs-config-path "history"))
(dolist (var '(kill-ring
               search-ring
               regexp-search-ring
               read-expression-history
               shell-command-history
               extended-command-history
               *~execute-text-prompt-hist*))
  (add-to-list 'savehist-additional-variables var))

;;
;; Enhanced find-file
;;

(defadvice find-file (around find-files activate)
  "Also finds all files within a list of files.  This even works
recursively."
  (if (listp filename)
      (cl-loop for f in filename do (find-file f wildcards))
    ad-do-it))

;;
;; Project management
;;

(require 'project)
;; (use-package project
;;   :ensure (:wait t))

;;
;; Enhanced M-x
;;
;; Ref: https://github.com/DarwinAwardWinner/amx
;;

(use-package amx
  :ensure t
  :config
  (setq amx-save-file (~get-rmacs-config-path "amx-items"))
  :init
  (amx-mode 1))

;;
;; Fuzzy finding, vertical completion, auto-completion framework, enhanced
;; minibuffer experience
;;
;; Ref: https://github.com/minad/vertico
;; Ref: https://github.com/minad/consult
;; Ref: https://github.com/minad/orderless
;; Ref: https://github.com/minad/corfu
;; Ref: https://github.com/minad/cape
;;

;; (use-package flx-ido)

(use-package emacs
  :custom
  (context-menu-mode t)
  ;; Minibuffer inside minibuffer (recursive editting) is quite useful
  (enable-recursive-minibuffers t)

  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Vertical display of candidates
(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode 1)
  (ido-mode -1)
  (savehist-mode 1))

(use-package consult
  :ensure t
  :demand t

  ;; Enable automatic preview at point in the *Completions* buffer, relevant
  ;; when using the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  ;; Show the completion candidates with Vertico
  (completion-in-region-function #'consult-completion-in-region)

  :init
  ;; Optionally configure the register formatting. This improves the
  ;; register preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.2
        register-preview-function #'consult-register-format)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (ido-mode -1)

  ;; Prompt indicator for `completing-read-multiple'.
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args)))))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))

  (setq consult-narrow-key "<")

  (defalias '~interactively-grep #'consult-ripgrep)
  (defalias '~interactively-call-symbol-menu #'consult-imenu)
  (defalias '~interactively-find-file #'find-file)
  (defalias '~interactively-find-file-in-project #'project-find-file)
  (defalias '~interactively-get-bookmarks #'consult-bookmark)
  (defalias '~interactively-yank-pop #'consult-yank-pop)
  (defalias '~interactively-search #'consult-line))

;; Auto-completion
(use-package corfu
  :ensure t
  :demand t

  :custom
  ;; Cycling
  (corfu-cycle t)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  :config
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)

  ;; Automatic popup, quit when there is no match
  (setq corfu-auto t
        corfu-quit-no-match nil))

;; Completion candidates
(use-package company
  :ensure t
  :demand t)
(use-package cape
  :ensure t
  :demand t
  :after (company yasnippet tempel)

  :hook ((prog-mode . ~cape-setup-general)
         (emacs-lisp-mode . ~cape-setup-general)
         (eglot-mode . ~cape-setup-eglot)
         (org-mode . ~cape-setup-general))

  :init
  ;; Super-Capf/capfs merging is nice with static, non- multi-stage completion
  ;; functions.  Check Cape's docs.
  (defun ~cape-capfs-static ()
    (cape-wrap-super #'cape-dabbrev
                     #'cape-abbrev
                     #'cape-dict
                     #'cape-keyword
                     #'cape-emoji
                     #'cape-sgml
                     #'cape-rfc1345
                     #'cape-tex))

  (cl-defun ~cape-load-capfs (&rest xs)
    (cl-loop for x in xs
             do (add-hook 'completion-at-point-functions x)))

  (cl-defun ~cape-setup-general ()
    (~cape-load-capfs #'tags-completion-at-point-function
                      #'cape-file
                      #'~cape-capfs-static
                      (cape-company-to-capf #'company-yasnippet)))

  ;; Ref: https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (cl-defun ~cape-setup-eglot ()
    (~cape-setup-general)
    (~cape-load-capfs (cape-capf-super #'eglot-completion-at-point
                                       #'tempel-expand)))

  :config
  (bind-key "M-SPC TAB" cape-prefix-map)

  ;; Continuously update the candidate when using Eglot
  ;; Ref: https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Enhanced matching
(use-package orderless
  :ensure t
  :demand t

  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))
  (setq completion-styles '(orderless basic)

        completion-category-overrides
        '((command (styles orderless+initialism))
          (symbol (styles orderless+initialism))
          (variable (styles orderless+initialism))
          (file (styles partial-completion)))

        ;; Emacs 31: partial-completion behaves like substring
        completion-pcm-leading-wildcard t))

;; Rich annotations in the minibuffer
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :demand t

  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  :custom
  ;; We don't care about the relative age
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)

  :init
  ;; Marginalia must be activated in the :init section of use-package such
  ;; that the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode 1))

;;
;; Temporary save points/bookmarks
;;
;; Ref: https://github.com/alezost/point-pos.el
;;

(use-package point-pos
  :ensure t)

;;
;; Multiple cursors
;;
;; Ref: https://github.com/magnars/multiple-cursors.el
;;

(use-package multiple-cursors
  :ensure t)

;;
;; Live doc in echo area
;;

(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          ielm-mode) . eldoc-mode)
  :config (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;;
;; Auto-pairing brackets
;;
;; Ref: https://github.com/Fuco1/smartparens
;;

;; Cheat sheet: M-x sp-cheat-sheet
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode 1))

;;
;; Adjust indentation based on current file
;;
;; Ref: https://github.com/jscheid/dtrt-indent
;;

(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-global-mode 1))

;;
;; Showing vertical guides
;;
;; Ref: https://github.com/jdtsmith/indent-bars
;;

(use-package indent-bars
  :ensure t

  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))

  :hook ((python-base-mode yaml-mode nickel-mode lisp-mode) . indent-bars-mode))

;;
;; Char-based navigation
;;
;; Ref: https://www.emacswiki.org/emacs/FastNav
;;

(use-package fastnav
  :ensure t
  :config
  (defvaralias 'lazy-highlight-face 'isearch-lazy-highlight))

;;
;; Bracket-based structured editing
;;
;; Ref: https://github.com/abo-abo/lispy
;;

(use-package lispy
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          clojure-mode
          cider-repl-mode
          sly-mrepl-mode
          slime-repl-mode) . lispy-mode)
  :ensure t
  :config
  (defun ~conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook #'~conditionally-enable-lispy)

  (defun ~lispy-update-keybindings ()
    (define-key lispy-mode-map (kbd "C-e") #'~my/activate-modalka)
    (define-key lispy-mode-map (kbd "C-a") #'~my/deactivate-modalka)
    (define-key lispy-mode-map (kbd "<M-return>") #'eval-defun)
    (define-key lispy-mode-map (kbd "<M-RET>") #'eval-defun)
    (define-key lispy-mode-map (kbd "<C-return>") #'~eval-last-sexp-or-region)
    (define-key lispy-mode-map (kbd "<C-RET>") #'~eval-last-sexp-or-region))
  (add-hook 'lisp-mode-hook #'~lispy-update-keybindings)

  (~lispy-update-keybindings))

;;
;; TODO: keybinding
;; * Text object manipulation
;; * Mark inner/outer <, (, [, {, \', \"
;;

;;
;; Version control systems: Mercurial and Git
;;
;; Ref: https://github.com/ananthakumaran/monky
;; Ref: https://github.com/magit/magit
;;

(use-package monky
  :ensure t
  :commands monky-status)

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after (transient)
  :commands (magit-status magit-get-top-dir)
  :init
  (setf magit-push-always-verify 'pp)
  ;; (setf git-commit-check-style-conventions nil)
  (setf git-commit-finish-query-functions nil))

;;
;; Pattern-based command execution
;;
;; Ref: https://github.com/cmpitg/wand
;;

;; TODO: Separate ~smart-open-file to a module

(use-package wand
  :ensure t
  :after (rmacs:config-module-bowser rmacs:config-module-convenient-buffer-shell f)
  :config
  (cl-defun ~wand:set-current-dir (&optional
                                   (text (thing-at-point 'line)))
    (interactive)
    (let ((text (string-trim text)))
      (when (f-dir? text)
        (setq-local default-directory text))))

  (cl-defun ~wand:open-or-eval (&optional text)
    "Performs an action based on what `text' represents:
- if `text' is a path to a directory, expands or collapses it with Bowser;
- if `text' is a file pattern, smartly opens it with `~smart-open-file';
- otherwise, executes it as Emacs Lisp code"
    (interactive)
    (let ((text (string-trim text)))
      (cond
       ((s-starts-with? "ssh://" text)
        (if (s-ends-with? "/" text)
            (bowser:expand-or-collapse-dir)
          (let ((ssh-expr (substring text (length "ssh://"))))
            (find-file (format "/ssh:%s" ssh-expr)))))
       ((~file-pattern? text)
        (if (and (string-equal text (bowser:get-path-current-line)) ;; Current line is a path
                 (f-exists? text))
            ;; Expand or collapse dir if is dir, or visit the file in
            ;; another frame if not.  This effectively makes it possible to
            ;; use Bowser as a poor man's file browser.
            (if (f-directory? text)
                (bowser:expand-or-collapse-dir)
              (find-file-other-window text)
              ;; (~find-file-in-previous-frame text)
              )
          (~smart-open-file text)))
       (t
        (wand:eval-string text)))))

  (cl-defun ~bs:exec-output-to-next-line (text)
    (interactive)
    (~open-line 1)
    (beginning-of-line)
    (bs:exec text))

  (defun ~build-|rmacs-tee-cmd (cmd)
    "Builds command to pipe output to the current buffer using rmacs-tee."
    (format "{ exec-and-echo-stdin %s } |& env RMACS_BUFFER_NAME='%s' RMACS_SERVER_NAME='%s' rmacs-tee"
            cmd (buffer-name) server-name))

  (setq wand:*rules*
        (list (wand:create-rule :match (rx bol (0+ " ") "<")
                                :capture :after
                                :skip-comment nil
                                :action #'~exec-sh<)
              (wand:create-rule :match (rx bol (0+ " ") "$<")
                                :capture :after
                                :skip-comment nil
                                :action #'bs:send-complete-string)
              (wand:create-rule :match (rx bol (0+ " ") "$")
                                :capture :after
                                :skip-comment nil
                                :action (~add-arg-to-history-fn *~exec-history-path* #'~bs:exec-output-to-next-line
                                                                :max-history *~exec-history-max*))
              (wand:create-rule :match (rx bol (0+ " ") ">")
                                :capture :after
                                :skip-comment nil
                                :action #'~exec-sh>)
              (wand:create-rule :match (rx bol (0+ " ") "!!!#")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~dispatch-action (concat "!!! " text))))
              (wand:create-rule :match (rx bol (0+ " ") "!!!")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~prepare-for-output-block t)
                                            (~dispatch-action (concat "!!! " (~build-|rmacs-tee-cmd text)))))
              (wand:create-rule :match (rx bol (0+ " ") "!#")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~dispatch-action (concat "!# " text))))
              (wand:create-rule :match (rx bol (0+ " ") "!@")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~dispatch-action (concat "!@ " text))))
              (wand:create-rule :match (rx bol (0+ " ") "!^")
                                :capture :after
                                :skip-comment nil
                                :action #'~exec-sh-pop-up)
              (wand:create-rule :match (rx bol (0+ " ") "!!#")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~dispatch-action (concat "!! " text))))
              (wand:create-rule :match (rx bol (0+ " ") "!!")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~prepare-for-output-block t)
                                            (~dispatch-action (concat "!! " (~build-|rmacs-tee-cmd text)))))
              (wand:create-rule :match (rx bol (0+ " ") "!")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~exec-sh<-next-line-separate text
                                                                          :callback #'(lambda (&rest _args)
                                                                                        (end-of-thing 'wand-text)
                                                                                        (forward-line)
                                                                                        (call-interactively #'~mark-current-output-block)))))
              (wand:create-rule :match (rx bol (0+ " ") "mux://")
                                :capture :after
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (~add-to-history-file *~exec-history-path* text :max-history *~exec-history-max*)
                                            (~dispatch-action (concat "mux://" text))))
              (wand:create-rule :match (rx bol (0+ " ") "ssh://"
                                           (1+ (not (any "!")))
                                           "!")
                                :capture :whole
                                :skip-comment nil
                                :action #'(lambda (text)
                                            (let ((cmd (thread-last (~split-string "!" text)
                                                                    rest
                                                                    (s-join "!")
                                                                    string-trim)))
                                              (~add-to-history-file *~exec-history-path* cmd :max-history *~exec-history-max*))
                                            ;; TODO Refactor - after the extraction of the display function from ~exec-sh<-next-line-separate
                                            (~exec-sh<-next-line-separate (format "dispatch-action %s"
                                                                                  (shell-quote-argument text)))))
              (wand:create-rule :match "----\n[^ ]* +"
                                :capture :after
                                :skip-comment nil
                                :action #'~current-snippet->file)
              (wand:create-rule :match (rx bol (0+ " ") "chrome:")
                                :capture :after
                                :action #'~open-with-google-chrome)
              (wand:create-rule :match (rx bol (0+ " ") "https?://")
                                :capture :whole
                                :action #'~web-browse-gui)
              ;; (wand:create-rule :match ".*\\.html$"
              ;;                   :capture :whole
              ;;                   :skip-comment nil
              ;;                   :action #'~web-browse-gui)
              (wand:create-rule :match (rx bol (0+ " ") "in:")
                                :capture :after
                                :action #'~wand:set-current-dir)
              (wand:create-rule :match (rx bol (0+ " ") "file:")
                                :capture :after
                                :action #'~smart-open-file)
              (wand:create-rule :match (rx (0+ (or any "\n")))
                                :capture :whole
                                :skip-comment nil
                                :action #'~wand:open-or-eval))))

;;
;; Snippet mode
;;
;; Ref: https://github.com/joaotavora/yasnippet
;; Ref: https://github.com/minad/tempel
;;
;; Note: Load before auto complete
;;

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name rmacs:+snippet-dir+))
  (yas-global-mode 1))

(use-package tempel
  :ensure t)
(use-package tempel-collection
  :ensure t
  :after (tempel))

;;
;; Showing color based on hex code
;;

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

;;
;; Always load man mode
;;

(use-package man)

;;
;; Some safe local variables
;;

(add-to-list 'safe-local-variable-values '(local/delete-on-close . t))
(add-to-list 'safe-local-variable-values '(local/delete-frame-on-close . t))
(add-to-list 'safe-local-variable-values '(local/delete-window-on-close . t))
(add-to-list 'safe-local-variable-values '(eval modify-syntax-entry 43 "'"))
(add-to-list 'safe-local-variable-values '(eval modify-syntax-entry 36 "'"))
(add-to-list 'safe-local-variable-values '(eval modify-syntax-entry 126 "'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core components for editing")

(provide 'rmacs:config-core-edit)
