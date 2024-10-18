;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2018-2024 Ha-Duong Nguyen (@cmpitg)
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
;; String processing
;;
;; Ref: https://github.com/magnars/s.el
;;

(use-package s)

;;
;; File/filesystem library
;;
;; Ref: https://github.com/rejeep/f.el
;;

(use-package f)

;;
;; Async processing by spawning subordinate processes
;;
;; Ref: https://github.com/jwiegley/emacs-async
;;

(use-package async)

;;
;; Editable grep'ing
;;

(use-package wgrep)

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
  :commands (er/expand-region
             er/mark-outside-pairs
             er/mark-outside-quotes
             er/mark-defun)
  :bind
  (("s-=" . #'er/expand-region)))

;;
;; Interactive menu
;;
;; Ref: https://github.com/abo-abo/hydra
;;

(use-package hydra
  :disabled t)

;;
;; grep command
;;

;; (setq grep-command "grep --ignore-case --line-number --with-filename -e ")
(setq grep-command "rg --ignore-case --line-number --with-filename -e ")

;;
;; Save minibuffer history across sessions
;;
;; Ref: https://www.emacswiki.org/emacs/SaveHist
;;

(savehist-mode 1)
(setq savehist-file
      (format "~/.emacs.d/history.%s" server-name))
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
      (loop for f in filename do (find-file f wildcards))
    ad-do-it))

;;
;; Project management
;;
;; Ref: https://github.com/bbatsov/projectile
;;
;; TODO: Check filtering & customization
;;

(use-package project
  :demand t)

(use-package projectile
  ;; :disabled t
  :diminish projectile-mode
  :init
  (progn
    (custom-set-variables `(projectile-known-projects-file ,(format (expand-file-name "projectile-bookmarks.%s.eld"
                                                                                      user-emacs-directory)
                                                                    server-name))))
  :config
  (progn
    (projectile-mode)

    (setq ~project-ignored-patterns
          (list (rx (0+ any) ".gz" eol)
                (rx (0+ any) ".pyc" eol)
                (rx (0+ any) ".jar" eol)
                (rx (0+ any) ".tar.gz" eol)
                (rx (0+ any) ".tgz" eol)
                (rx (0+ any) ".zip" eol)
                (rx (0+ any) ".pyc" eol)
                (rx (0+ any) ".elc" eol)
                (rx (0+ any) ".class" eol)
                (rx (0+ any) "~" eol)
                (rx (0+ any) "swp" eol)
                (rx ".lein-" (0+ any))
                (rx ".git/")
                (rx ".hg/")
                (rx "/classes/")
                (rx "/target/")
                (rx "/node_modules/")
                (rx "/bower_components/")))

    (defun ~projectile-ignored-patterns ()
      "Collects all ignored patterns for Projectile."
      (concatenate 'list ~project-ignored-patterns
                   (first (projectile-filtering-patterns))))

    (defun ~projectile-ignored? (path)
      (cl-member-if #'(lambda (pattern)
                        (string-match pattern path))
                    (~projectile-ignored-patterns)))

    (defun ~print-files-advice-around (orig-fun &rest args)
      (let* ((files (apply orig-fun args))
             (filtered-with-regex (cl-remove-if #'~projectile-ignored? files)))
        filtered-with-regex))

    (advice-add 'projectile-remove-ignored :around #'~print-files-advice-around)
    ;; (advice-remove 'projectile-remove-ignored #'~print-files-advice-around)

    ;; Don't use truename, e.g. don't follow symlinks
    ;; Ref:
    ;; * https://github.com/bbatsov/projectile/pull/566
    ;; * https://github.com/bbatsov/projectile/issues/1387
    ;; * https://github.com/bbatsov/projectile/issues/1404
    (defun ~dont-use-truename-projectile-root
        (old-fn &rest args)
      (cl-flet ((file-truename (f) f))
        (apply old-fn args)))
    (advice-add 'projectile-project-root :around #'~dont-use-truename-projectile-root)
    (advice-add 'projectile-find-file :around #'~dont-use-truename-projectile-root)
    (advice-add 'project-find-file :around #'~dont-use-truename-projectile-root)
    (advice-add 'project-current :around #'~dont-use-truename-projectile-root)

    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-find-dir-includes-top-level t)
    (setq projectile-enable-caching t)

    (setq projectile-indexing-method 'hybrid)

    ;; (setq projectile-indexing-method 'alien)
    (setq projectile-generic-command "fdfind . --type f | grep -v -f <(grep . .projectile) | tr \"\\n\" \"\\0\"")))

;;
;; Enhanced M-x
;;
;; Ref: https://github.com/DarwinAwardWinner/amx
;;

(use-package amx
  :config
  (progn
    (amx-mode 1)
    (setq amx-save-file (format "~/.emacs.d/amx-items.%s" server-name))))

;;
;; Fuzzy finding
;;

;; (use-package flx-ido)

(use-package vertico
  :init
  (progn
    (vertico-mode 1)
    (ido-mode -1)
    (savehist-mode 1)))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer, relevant
  ;; when using the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (progn
    (ido-mode -1)
    ;; Optionally configure the register formatting. This improves the
    ;; register preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

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
    (defalias '~interactively-search #'consult-line)

    ))

;;
;; Temporary save points
;;
;; Ref: https://github.com/alezost/point-pos.el
;;

(use-package point-pos)

;;
;; Multiple cursors
;;
;; Ref: https://github.com/magnars/multiple-cursors.el
;;

(use-package multiple-cursors)

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
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)))

;;
;; Adjust indentation based on current file
;;
;; Ref: https://github.com/jscheid/dtrt-indent
;;

(use-package dtrt-indent
  :config (dtrt-indent-global-mode 1))

;;
;; Char-based navigation
;;
;; Ref: https://www.emacswiki.org/emacs/FastNav
;;

(use-package fastnav
  :config (progn
            (defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)))

;;
;; Bracket-based structured editing
;;
;; Ref: https://www.emacswiki.org/emacs/ParEdit
;; Ref: https://github.com/DogLooksGood/parinfer-mode
;; Ref: https://github.com/abo-abo/lispy
;;

(use-package parinfer
  :disabled t
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          clojure-mode
          cider-repl-mode
          sly-mrepl-mode
          slime-repl-mode) . parinfer-mode)
  :bind
  (("C-a" . #'parinfer-toggle-mode))
  :config
  (progn
    (setq parinfer-extensions
          '(defaults
             pretty-parens
             ;; evil
             ;; lispy
             smart-tab
             smart-yank))

    (defun ~parinfer-update-keybindings ()
      (define-key parinfer-mode-map (kbd "C-e") #'~my/activate-modalka)
      (define-key parinfer-mode-map (kbd "C-a") #'~my/deactivate-modalka)
      (define-key parinfer-mode-map (kbd "C-'") #'parinfer-toggle-mode)
      (define-key parinfer-mode-map (kbd "<M-return>") #'eval-defun)
      (define-key parinfer-mode-map (kbd "<M-RET>") #'eval-defun)
      (define-key parinfer-mode-map (kbd "<C-return>") #'~eval-last-sexp-or-region)
      (define-key parinfer-mode-map (kbd "<C-RET>") #'~eval-last-sexp-or-region))
    (add-hook 'lisp-mode-hook #'~parinfer-update-keybindings)

    (~parinfer-update-keybindings)))

(use-package lispy
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          clojure-mode
          cider-repl-mode
          sly-mrepl-mode
          slime-repl-mode) . lispy-mode)
  :demand t
  :config
  (progn
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

    (~lispy-update-keybindings)))

(use-package paredit
  :disabled t
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          clojure-mode
          cider-repl-mode
          sly-mrepl-mode
          slime-repl-mode) . paredit-mode)
  :bind (:map
         paredit-mode-map
         ("s-." . #'paredit-backward-kill-word)
         ("s-p" . #'paredit-forward-kill-word)
         ("s-r" . #'forward-word)
         ("s-g" . #'backward-word)
         ("s-C" . #'paredit-backward-up)
         ("s-T" . #'paredit-forward-up)
         ("s-R" . #'paredit-forward)
         ("s-G" . #'paredit-backward))
  :defines (slime-repl-mode-map sly-mrepl-mode-map)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meow sensible modal editting, inspired by Kakoune
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ref: https://github.com/meow-edit/meow/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package meow
  :config
  (progn
    (defun meow-setup-qwerty ()
      (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
      (meow-motion-overwrite-define-key
       '("j" . meow-next)
       '("k" . meow-prev)
       '("<escape>" . ignore))
      (meow-leader-define-key
       ;; SPC j/k will run the original command in MOTION state.
       '("j" . "H-j")
       '("k" . "H-k")
       ;; Use SPC (0-9) for digit arguments.
       '("1" . meow-digit-argument)
       '("2" . meow-digit-argument)
       '("3" . meow-digit-argument)
       '("4" . meow-digit-argument)
       '("5" . meow-digit-argument)
       '("6" . meow-digit-argument)
       '("7" . meow-digit-argument)
       '("8" . meow-digit-argument)
       '("9" . meow-digit-argument)
       '("0" . meow-digit-argument)
       '("/" . meow-keypad-describe-key)
       '("?" . meow-cheatsheet))
      (meow-normal-define-key
       '("0" . meow-expand-0)
       '("9" . meow-expand-9)
       '("8" . meow-expand-8)
       '("7" . meow-expand-7)
       '("6" . meow-expand-6)
       '("5" . meow-expand-5)
       '("4" . meow-expand-4)
       '("3" . meow-expand-3)
       '("2" . meow-expand-2)
       '("1" . meow-expand-1)
       '("-" . negative-argument)
       '(";" . meow-reverse)
       '("," . meow-inner-of-thing)
       '("." . meow-bounds-of-thing)
       '("[" . meow-beginning-of-thing)
       '("]" . meow-end-of-thing)
       '("a" . meow-append)
       '("A" . meow-open-below)
       '("b" . meow-back-word)
       '("B" . meow-back-symbol)
       '("c" . meow-change)
       '("d" . meow-delete)
       '("D" . meow-backward-delete)
       '("e" . meow-next-word)
       '("E" . meow-next-symbol)
       '("f" . meow-find)
       '("g" . meow-cancel-selection)
       '("G" . meow-grab)
       '("h" . meow-left)
       '("H" . meow-left-expand)
       '("i" . meow-insert)
       '("I" . meow-open-above)
       '("j" . meow-next)
       '("J" . meow-next-expand)
       '("k" . meow-prev)
       '("K" . meow-prev-expand)
       '("l" . meow-right)
       '("L" . meow-right-expand)
       '("m" . meow-join)
       '("n" . meow-search)
       '("o" . meow-block)
       '("O" . meow-to-block)
       '("p" . meow-yank)
       '("q" . meow-quit)
       '("Q" . meow-goto-line)
       '("r" . meow-replace)
       '("R" . meow-swap-grab)
       '("s" . meow-kill)
       '("t" . meow-till)
       '("u" . meow-undo)
       '("U" . meow-undo-in-selection)
       '("v" . meow-visit)
       '("w" . meow-mark-word)
       '("W" . meow-mark-symbol)
       '("x" . meow-line)
       '("X" . meow-goto-line)
       '("y" . meow-save)
       '("Y" . meow-sync-grab)
       '("z" . meow-pop-selection)
       '("'" . repeat)
       '("<escape>" . ignore)))
    (meow-setup-qwerty)
    (setq meow-use-clipboard t)
    (custom-set-faces
     '(meow-grab ((t (:inherit secondary-selection))))
     '(meow-normal-indicator ((t ())))
     '(meow-motion-indicator ((t ())))
     '(meow-keypad-indicator ((t ())))
     '(meow-insert-indicator ((t ()))))
    ;; (add-to-list 'meow-expand-exclude-mode-list 'dired-mode)
    ;; (add-to-list 'meow-expand-exclude-mode-list 'wdired-mode)
    ;; (add-to-list 'meow-mode-state-list '(magit-mode . insert))
    (add-to-list 'meow-mode-state-list '(magit-mode . normal))
    (add-to-list 'meow-mode-state-list '(dired-mode . normal))
    (add-to-list 'meow-mode-state-list '(wdired-mode . normal))
    (meow-setup-indicator)
    ;; For some reasons cua-mode breaks meow-reverse command
    (when (and (boundp 'cua-mode) cua-mode)
      (cua-mode -1))
    (meow-global-mode 1)))

;;
;; TODO: keybinding
;; * Text object manipulation
;; * Mark inner/outer <, (, [, {, \', \"
;;

;;
;; Auto completion framework
;;
;; Ref: https://github.com/company-mode/company-mode
;;

(use-package company
  :diminish company-mode
  :bind (:map company-mode-map
         ("C-/" . #'company-complete))
  :demand t
  :config (progn
            (global-company-mode 1)
            (use-package pos-tip)))
(use-package company-quickhelp
  :demand t
  :bind (:map company-active-map
         ("M-h" . #'company-quickhelp-manual-begin))
  :config (progn
            (company-quickhelp-mode 1)
            ;; Do not trigger automatically
            (setq company-quickhelp-delay nil)))

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
    (setf git-commit-finish-query-functions nil)))

;; Ref: https://github.com/emacs-evil/evil-magit
;; (use-package evil-magit
;;   :after (evil magit))

;;
;; Pattern-based command execution
;;
;; Ref: https://github.com/cmpitg/wand
;;

;; TODO: Separate ~smart-open-file to a module

(use-package wand
  :after (rmacs:config-module-bowser rmacs:config-module-convenient-buffer-shell f)
  :config
  (progn
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
                (wand:create-rule :match ".*\\.html$"
                                  :capture :whole
                                  :skip-comment nil
                                  :action #'~web-browse-gui)
                (wand:create-rule :match (rx bol (0+ " ") "in:")
                                  :capture :after
                                  :action #'~wand:set-current-dir)
                (wand:create-rule :match (rx bol (0+ " ") "file:")
                                  :capture :after
                                  :action #'~smart-open-file)
                (wand:create-rule :match (rx (0+ (or any "\n")))
                                  :capture :whole
                                  :skip-comment nil
                                  :action #'~wand:open-or-eval)))))

;;
;; Snippet mode
;;
;; Ref: https://github.com/joaotavora/yasnippet
;;
;; Note: Load before auto complete
;;

(use-package yasnippet
  :diminish yas-minor-mode
  :config (progn
            (add-to-list 'yas-snippet-dirs (expand-file-name *snippet-dir*))
            (yas-global-mode 1)))

;;
;; Showing color based on hex code
;;

(use-package rainbow-mode
  :commands (rainbow-mode))

;;
;; Always load man mode
;;

(use-package man)

;;
;; Modalka in other modes
;;

(with-eval-after-load "compilation"
  (define-key compilation-mode-map (kbd "C-e") #'~my/activate-modalka)
  (define-key compilation-mode-map (kbd "C-a") #'~my/deactivate-modalka))

(require 'dired)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-e") #'~my/activate-modalka)
  (define-key dired-mode-map (kbd "C-a") #'~my/deactivate-modalka))

;; Some safe local variables
(add-to-list 'safe-local-variable-values '(local/delete-on-close . t))
(add-to-list 'safe-local-variable-values '(local/delete-frame-on-close . t))
(add-to-list 'safe-local-variable-values '(local/delete-window-on-close . t))
(add-to-list 'safe-local-variable-values '(eval modify-syntax-entry 43 "'"))
(add-to-list 'safe-local-variable-values '(eval modify-syntax-entry 36 "'"))
(add-to-list 'safe-local-variable-values '(eval modify-syntax-entry 126 "'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core components for editing")

(provide 'rmacs:config-core-edit)
