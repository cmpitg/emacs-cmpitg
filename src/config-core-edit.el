;;
;; Copyright (C) 2018-2020 Ha-Duong Nguyen (@cmpitg)
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

(use-package tramp
  :config
  (progn
    ;; Tramp hangs due to fancy shell prompt -
    ;; https://www.emacswiki.org/emacs/TrampMode#toc12
    (custom-set-variables `(tramp-default-method "ssh")
                          `(tramp-terminal-type "dumb")
                          `(tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*"))))

;;
;; Expand selection with one keybinding
;;
;; Ref: https://github.com/magnars/expand-region.el
;;

(use-package expand-region
  :commands (er/expand-region
             er/mark-outside-pairs
             er/mark-outside-quotes
             er/mark-defun))

;;
;; Interactive menu
;;
;; Ref: https://github.com/abo-abo/hydra
;;

(use-package hydra
  :demand t)

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

    (defun ~find-files-current-dir ()
      "Activates `projectile-find-files', taking current directory as
project root."
      (interactive)
      ;; Ignore the obsolete, we do need the powerful dynamic binding capability
      ;; of flet that neither cl-flet nor cl-letf provides
      (cl-flet ((projectile-project-root () default-directory)
                (projectile-current-project-files
                 ()
                 (let (files)
                   (setq files (-mapcat #'projectile-dir-files
                                        (projectile-get-project-directories)))
                   (projectile-sort-files files))))
        (call-interactively 'projectile-find-file)))

    (defun ~find-files-current-dir-not-ignoring ()
      "Activates `projectile-find-files', taking current directory as
project root, not ignoring anything."
      (interactive)
      ;; Ignore the obsolete, we do need the powerful dynamic binding capability
      ;; of flet that neither cl-flet nor cl-letf provides
      (let ((projectile-generic-command "fdfind . --type f | tr \"\\n\" \"\\0\""))
        (cl-flet ((projectile-project-root () default-directory)
                  (projectile-current-project-files
                   ()
                   (let (files)
                     (setq files (-mapcat #'projectile-dir-files
                                          (projectile-get-project-directories)))
                     (projectile-sort-files files))))
          (call-interactively 'projectile-find-file))))

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
;; Ref: https://github.com/abo-abo/swiper
;;

(use-package flx-ido)

(use-package counsel
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
              ("M-s-c"      . ~ivy-prev-line+)
              ("M-s-t"      . ~ivy-next-line+)
              ("C-m"        . ivy-alt-done)
              ("<f3>"       . ivy-occur)
              ("<s-return>" . ivy-dispatching-done)
              ("<C-return>" . ivy-immediate-done)
              ("<S-return>" . ivy-call))
  :demand t
  :config (progn
            (ivy-mode 1)

            ;; Include recentf and bookmarks when switching buffers
            (setq ivy-use-virtual-buffers t)

            ;; Show 15 items
            (setq ivy-height 15)

            ;; Don't count the candidates
            (setq ivy-count-format "")

            ;; Plain display style works and is greener
            (setq ivy-display-style nil)

            ;; Use Projectile with Ivy
            (setq projectile-completion-system 'ivy)

            (setq enable-recursive-minibuffers t)

            (setq ivy-use-selectable-prompt t)

            (setf (nth 0 counsel-rg-base-command) (executable-find "rgg" "rg"))

            ;; Disable one-buffer-per-window behavior when swiping
            (with-eval-after-load 'rmacs:config-one-buffer-per-window
              (~disable-one-buffer-per-window-for '(swiper-all
                                                    swiper-all-query-replace
                                                    swiper-multi
                                                    swiper-query-replace)))

            (require 'flx)
            ;; Default matching
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
            (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
            (setq ivy-initial-inputs-alist nil)

            (cl-defun ~ivy-next-line+ (&optional (n-lines 5))
              (interactive)
              (ivy-next-line n-lines))

            (cl-defun ~ivy-prev-line+ (&optional (n-lines 5))
              (interactive)
              (ivy-previous-line n-lines))

            (cl-defun ~bind-key-with-prefix (key command &key
                                                 (keymap global-map))
              "Binds key in `evil-normal-state-map' and `evil-visual-state-map' with prefix `SPC' and in global mode map with prefix `s-SPC' at the same time."
              (interactive)
              (eval `(progn
                       (bind-key ,(format "s-SPC %s" key) command keymap))))

            (cl-defun ~bind-key-with-prefix-local (key command &key (keymap global-map))
              "Like `~bind-key-with-prefix', except that the binding is local."
              (interactive)
              (eval `(progn
                       (bind-key ,(format "s-SPC %s" key) command keymap))))

            (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

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
      (define-key parinfer-mode-map (kbd "C-o") #'~my/deactivate-modalka)
      (define-key parinfer-mode-map (kbd "C-a") #'parinfer-toggle-mode)
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
      (define-key lispy-mode-map (kbd "C-o") #'~my/deactivate-modalka)
      (define-key lispy-mode-map (kbd "C-a") #'parinfer-toggle-mode)
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
  :bind (:map paredit-mode-map
              ("s-." . paredit-backward-kill-word)
              ("s-p" . paredit-forward-kill-word)
              ("s-r" . forward-word)
              ("s-g" . backward-word)
              ("s-C" . paredit-backward-up)
              ("s-T" . paredit-forward-up)
              ("s-R" . paredit-forward)
              ("s-G" . paredit-backward))
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

;;
;; Modal editing mode
;;
;; Ref: https://github.com/mrkkrp/modalka
;;

(use-package modalka
  :demand t
  ;; Must load after Lisp for the keybindings to take precendence
  ;; :after (lispy parinfer)
  :after (lispy)
  :bind* (:map
          modalka-mode-map
          ("SPC" . #'hydra-global/body)
          ("u" . #'forward-char)
          ("o" . #'backward-char)
          ("e" . #'next-line)
          ("." . #'previous-line)
          ("," . #'~backward-word-boundary)
          ("p" . #'~forward-word-boundary)
          ("a" . #'beginning-of-line)
          ("i" . #'end-of-line)

          ("J" . #'~join-with-next-line)
          ("c" . #'~replace-line)
          ("d" . #'kill-line)
          ("D" . #'~kill-whole-line)
          ("#" . #'er/expand-region)
          ("y" . #'~open-line)
          ("Y" . #'~open-line-before)

          ("v" . #'set-mark-command)
          ("V" . #'~select-line)
          ("w" . #'~select-word)
          ("mm" . #'~mark-current-output-block)

          ("s" . #'~search-buffer-interactively)
          ("/" . #'isearch-forward)
          ("?" . #'isearch-backward)
          ("r" . #'query-replace)
          ("R" . #'query-replace-regexp)
          ("h" . #'highlight-regexp)
          ("H" . #'unhighlight-regexp)
          ("z" . #'fastnav-sprint-forward)
          ("Z" . #'fastnav-sprint-backward)

          ("X" . #'~execute-current-wand-text)
          ("xx" . #'hydra-freeform-exec/body)
          ("I" . #'bowser:expand-dir-here)

          (">" . #'beginning-of-buffer)
          ("E" . #'end-of-buffer)
          (";" . #'kill-current-buffer)
          (":" . #'~kill-buffer-and-window)

          ("(" . #'kmacro-start-macro)
          (")" . #'kmacro-end-macro)
          ("@" . #'kmacro-call-macro)

          ("'" . #'undo-tree-undo)
          ("\"" . #'undo-tree-redo)
          ("q" . #'cua-cut-region)
          ("j" . #'cua-copy-region)
          ("k" . #'cua-paste)

          ("ff" . #'projectile-find-file)
          ("fo" . #'find-file)
          :map
          global-map
          ("C-e" . #'~my/activate-modalka)
          ("C-o" . #'~my/deactivate-modalka))
  :config
  (progn
    (setq modalka-cursor-type 'box)

    (defun ~my/activate-modalka ()
      (interactive)
      (modalka-mode 1))

    (defun ~my/deactivate-modalka ()
      (interactive)
      (modalka-mode -1))

    (defun ~my/magit-load-modalka ()
      "Loads Modalka in Magit status mode."
      (interactive)
      (modalka-mode -1)
      (modalka-mode 1))
    (add-hook 'magit-status-mode-hook #'~my/magit-load-modalka)
    (modalka-global-mode)))

(defun ~delete-line ()
  "Deletes the current line."
  (interactive)
  (delete-region (point-at-bol) (point-at-eol)))

(defun ~replace-line ()
  "Replaces the current line."
  (interactive)
  (~delete-line)
  (modalka-mode -1))

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
         ("C-/" . company-complete))
  :demand t
  :config (progn
            (global-company-mode 1)
            (use-package pos-tip)))
(use-package company-quickhelp
  :demand t
  :bind (:map company-active-map
         ("M-h" . company-quickhelp-manual-begin))
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

    (defvar *~command-pattern-regexp* (rx bol (0+ " ") (or "$" "!" "!^" "!@" "!!" "!!!" "mux://"))
      "Regexp that determines whether or not a string is a command pattern.")

    (defun ~goto-next-command-pattern ()
      "Goes to the next line matching one of the command patterns."
      (interactive)
      (re-search-forward *~command-pattern-regexp* nil t))

    (defun ~goto-prev-command-pattern ()
      "Goes to the previous line matching one of the command patterns."
      (interactive)
      (re-search-backward *~command-pattern-regexp* nil t))

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
                                              (let ((cmd (thread-last (s-split "!" text)
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
  (define-key compilation-mode-map (kbd "C-o") #'~my/deactivate-modalka))

(with-eval-after-load "dired"
  (define-key dired-mode-map (kbd "C-e") #'~my/activate-modalka)
  (define-key dired-mode-map (kbd "C-o") #'~my/deactivate-modalka))

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
