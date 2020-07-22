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

(use-package cl-lib)
(use-package misc)
(use-package thingatpt)
(use-package subr-x)

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
;; Enhacing different Things at point
;;

(put 'wand-text 'bounds-of-thing-at-point '~bounds-of-wand-text-at-point)

;;
;; Project management
;;
;; Ref: https://github.com/bbatsov/projectile
;;
;; TODO: Check filtering & customization
;;

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

            ;; Disable one-buffer-per-window behavior when swiping
            (with-eval-after-load 'rmacs:config-one-buffer-per-window
              (~disable-one-buffer-per-window-for '(swiper-all
                                                    swiper-all-query-replace
                                                    swiper-multi
                                                    swiper-query-replace)))

            (use-package flx)
            ;; Default matching
            (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus) (t . ivy--regex-fuzzy)))
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)
            ;;                               (t . ivy--regex-fuzzy)
            ;;                               (t . ivy--regex-ignore-order)))

            (defun* ~ivy-next-line+ (&optional (n-lines 5))
              (interactive)
              (ivy-next-line n-lines))

            (defun* ~ivy-prev-line+ (&optional (n-lines 5))
              (interactive)
              (ivy-previous-line n-lines))

            (defun ~bind-key-evil (key func)
              "Binds key in all evil states."
              (dolist (map (list evil-normal-state-map
                                 evil-insert-state-map
                                 evil-visual-state-map
                                 evil-replace-state-map
                                 evil-operator-state-map
                                 evil-motion-state-map))
                (bind-key key func map)))

            (defun* ~bind-key-with-prefix (key command &key
                                               (keymap global-map))
              "Binds key in `evil-normal-state-map' and
`evil-visual-state-map' with prefix `SPC' and in global mode map
with prefix `s-SPC' at the same time."
              (interactive)
              (eval `(progn
                       (bind-key ,(format "s-SPC %s" key) command keymap)
                       (evil-define-key 'normal keymap (kbd ,(format "SPC %s" key)) command)
                       (evil-define-key 'visual keymap (kbd ,(format "SPC %s" key)) command))))

            (defun* ~bind-key-with-prefix-local (key command &key (keymap global-map))
              "Like `~bind-key-with-prefix', except that the binding is local."
              (interactive)
              (eval `(progn
                       (bind-key ,(format "s-SPC %s" key) command keymap)
                       (bind-key ,(format "SPC %s" key) command evil-normal-state-local-map)
                       (bind-key ,(format "SPC %s" key) command evil-visual-state-local-map))))

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
(use-package smartparens-config
  :config (smartparens-global-mode))

;;
;; Adjust indentation based on current file
;;
;; Ref: https://github.com/jscheid/dtrt-indent
;;

(use-package dtrt-indent
  :config (dtrt-indent-mode 1))

;;
;; Vim mode
;;
;; Ref: https://github.com/emacs-evil/evil
;;

(use-package evil
  :demand t
  :bind (("M-ESC"     . '~keyboard-quit)
         :map evil-insert-state-map
         ("C-y"       . yank)
         ("C-o"       . ~open-line)
         ("C-w"       . kill-region)
         ("C-a"       . ~move-to-beginning-of-line)
         ("C-k"       . ~evil-kill-line-or-sexpr)
         ("<mouse-2>" . nil)
         :map evil-normal-state-map
         ("<mouse-2>" . nil)
         ("u" . undo-tree-undo)
         :map evil-visual-state-map
         ("<mouse-2>" . nil))
  :config (progn
            (evil-mode t)

            ;; Default evil mode
            (setq evil-default-state 'insert)
            ;; (setq evil-default-state 'normal)
            (setq-default evil-kill-on-visual-paste nil)

            ;; FFS
            ;; Ref: https://github.com/emacs-evil/evil/issues/1152
            (evil-define-command evil-visual-paste (count &optional register)
              "Paste over Visual selection."
              :suppress-operator t
              (interactive "*P<x>")
              (setq count (prefix-numeric-value count))
              ;; evil-visual-paste is typically called from evil-paste-before or
              ;; evil-paste-after, but we have to mark that the paste was from
              ;; visual state
              (setq this-command 'evil-visual-paste)
              (let* ((text (if register
                               (evil-get-register register)
                             (current-kill 0)))
                     (yank-handler (car-safe (get-text-property
                                              0 'yank-handler text)))
                     new-kill
                     paste-eob)
                (evil-with-undo
                  (let ((kill-ring-yank-pointer (list (current-kill 0))))
                    (when (evil-visual-state-p)
                      (evil-visual-rotate 'upper-left)
                      ;; if we replace the last buffer line that does not end in a
                      ;; newline, we use `evil-paste-after' because `evil-delete'
                      ;; will move point to the line above
                      (when (and (= evil-visual-end (point-max))
                                 (/= (char-before (point-max)) ?\n))
                        (setq paste-eob t))
                      (evil-delete evil-visual-beginning evil-visual-end (evil-visual-type) ?_)

                      (when (and (eq yank-handler #'evil-yank-line-handler)
                                 (not (eq (evil-visual-type) 'line))
                                 (not (= evil-visual-end (point-max))))
                        (insert "\n"))
                      (evil-normal-state)
                      (current-kill 1))
                    (if paste-eob
                        (evil-paste-after count register)
                      (evil-paste-before count register)))
                  (when evil-kill-on-visual-paste
                    (current-kill -1))
                  ;; mark the last paste as visual-paste
                  (setq evil-last-paste
                        (list (nth 0 evil-last-paste)
                              (nth 1 evil-last-paste)
                              (nth 2 evil-last-paste)
                              (nth 3 evil-last-paste)
                              (nth 4 evil-last-paste)
                              t)))))
            (evil-define-operator evil-delete-char (beg end type register)
              "Delete next character."
              :motion evil-forward-char
              (interactive "<R><x>")
              (if register (evil-delete beg end type register)
                (evil-delete beg end type ?_)))

            ;; (with-eval-after-load "evil-vars"
            ;;   (dolist (mode '(term-mode
            ;;                   multi-term-mode
            ;;                   ansi-term-mode
            ;;                   magit-log-edit-mode
            ;;                   magit-popup-mode
            ;;                   magit-file-mode
            ;;                   dired-mode
            ;;                   nav-mode
            ;;                   grep-mode
            ;;                   bs-mode
            ;;                   cider-repl-mode
            ;;                   cider-popup-buffer-mode
            ;;                   cider--debug-mode
            ;;                   cider-temp-mode
            ;;                   help-mode
            ;;                   compilation-mode
            ;;                   ivy-occur-mode))
            ;;     (evil-set-initial-state mode 'emacs))
            ;;   (evil-set-initial-state 'ibuffer-mode 'normal))

            (setq evil-emacs-state-cursor 'bar)
            (setq-default cursor-type 'bar)

            ;; Better granularity for undo-tree
            (setq evil-want-fine-undo t)

            ;; Down mouse 1 should change evil to insert mode
            ;; (defun ~advice/mouse-1-evil-insert-mode (orig-fun &rest args)
            ;;   (interactive)
            ;;   (let ((res (call-interactively orig-fun))
            ;;         (old-point (point)))
            ;;     (call-interactively 'evil-insert)
            ;;     ;; After calling evil-insert, the cursor moves the beginning of the region
            ;;     ;; so we need to set it back
            ;;     (when (< (point) old-point)
            ;;       (call-interactively 'exchange-point-and-mark))
            ;;     res))
            ;; (advice-add 'evil-mouse-drag-region :around #'~advice/mouse-1-evil-insert-mode)
            ;; (advice-remove 'evil-mouse-drag-region #'~advice/mouse-1-evil-insert-mode)

            (defun ~evil-kill-line-or-sexpr ()
              "Kills current line or sexp using Paredit in Evil mode."
              (interactive)
              (if (cl-member mode-name '("Emacs-Lisp" "Lisp" "Clojure") :test 'equalp)
                  (call-interactively 'paredit-kill)
                (call-interactively 'kill-line)))))

(use-package evil-collection
  :after (evil company-mode)
  :demand t
  :init (evil-collection-init))

(evil-mode 1)
;; * to search forward, # to search backward
(use-package evil-visualstar
  :after (evil)
  :init (progn
          (global-evil-visualstar-mode)
          (setq evil-visualstar/persistent nil)))

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
(use-package evil-magit
  :after (evil magit))

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
    (defun* ~wand:set-current-dir (&optional
                                   (text (thing-at-point 'line)))
      (interactive)
      (let ((text (string-trim text)))
        (when (f-dir? text)
          (setq-local default-directory text))))

    (defun* ~wand:open-or-eval (&optional text)
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
          (if (and (string-equal text (bowser:get-path-current-line))
                   (f-directory? text)
                   (f-exists? text))
              (bowser:expand-or-collapse-dir)
            (~smart-open-file text)))
         (t
          (wand:eval-string text)))))

    (defun* ~bs:exec-output-to-next-line (text)
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
      (format "%s |& rmacs-tee" cmd server-name))

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
                                              (~dispatch-action (~build-|rmacs-tee-cmd (concat "!!! " text)))))
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
                                              (~dispatch-action (~build-|rmacs-tee-cmd (concat "!! " text)))))
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
