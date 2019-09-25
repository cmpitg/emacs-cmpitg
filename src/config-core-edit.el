;;
;; Copyright (C) 2018-2019 Ha-Duong Nguyen (@cmpitg)
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
;; List processing, use with care since it's generally slower than Emacs Lisp primitives
;;
;; Ref: https://github.com/magnars/dash.el
;;

(use-package dash
  :disabled t
  :config (dash-enable-font-lock))

;;
;; File/filesystem library
;;
;; Ref: https://github.com/rejeep/f.el
;;

(use-package f)

;;
;; Hashtable processing
;;
;; Ref: https://github.com/Wilfred/ht.el
;;

(use-package ht)

;;
;; Async processing by spawning subordinate processes
;;
;; Ref: https://github.com/jwiegley/emacs-async
;;

(use-package async)

;;
;; Threading (Yay!)
;;
;; Ref: https://github.com/mola-T/timp
;;

(use-package timp)

;;
;; Tramp for remote & sudo access
;;

(use-package tramp)

;;
;; Expand selection with one keybinding
;;
;; Ref: https://github.com/magnars/expand-region.el
;;

(use-package expand-region
  :commands er/expand-region)

;;
;; Asciidoc mode
;;
;; Ref: https://github.com/sensorflo/adoc-mode
;;

(use-package adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode)
  :after (evil)
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

    (~bind-key-with-prefix "d r" #'~asciidoc/render :keymap adoc-mode-map)
    (~bind-key-with-prefix "d p" #'~asciidoc/preview :keymap adoc-mode-map)
    (~bind-key-with-prefix "d u" #'~asciidoc/update-preview :keymap adoc-mode-map)))
(setq-default initial-major-mode 'adoc-mode)
(setq-default major-mode 'adoc-mode)

;;
;; grep command
;;

(setq grep-command "grep -i -nH -e ")

;;
;; Save minibuffer history across sessions
;;
;; Ref: https://www.emacswiki.org/emacs/SaveHist
;;

(savehist-mode 1)
(setq savehist-file
      (format "~/.emacs.d/history.%s"
              (~emacs-server-name)))
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
;; Project management
;;
;; Ref: https://github.com/bbatsov/projectile
;;

(use-package projectile
  :diminish projectile-mode
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

    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-find-dir-includes-top-level t)
    (setq projectile-enable-caching t)

    ;; (setq projectile-indexing-method 'hybrid)

    (setq projectile-indexing-method 'alien)
    (setq projectile-generic-command "find . -type f | grep -v -f <(grep . .projectile) | tr \"\\n\" \"\\0\"")))

;;
;; Enhanced M-x
;;
;; Ref: https://github.com/DarwinAwardWinner/amx
;;

(use-package amx
  :config
  (progn
    (amx-mode 1)
    (setq amx-save-file (format "~/.emacs.d/amx-items.%s"
                                (~emacs-server-name)))))

;;
;; Smart completion framework
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
;; Temporarily save points
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
            (defun ~advice/mouse-1-evil-insert-mode (orig-fun &rest args)
              (interactive)
              (let ((res (call-interactively orig-fun))
                    (old-point (point)))
                (call-interactively 'evil-insert)
                ;; After calling evil-insert, the cursor moves the beginning of the region
                ;; so we need to set it back
                (when (< (point) old-point)
                  (call-interactively 'exchange-point-and-mark))
                res))
            ;; (advice-add 'evil-mouse-drag-region :around #'~advice/mouse-1-evil-insert-mode)
            ;; (advice-remove 'evil-mouse-drag-region #'~advice/mouse-1-evil-insert-mode)

            (defun ~evil-kill-line-or-sexpr ()
              "Kills current line or sexp using Paredit in Evil mode."
              (interactive)
              (if (cl-member mode-name '("Emacs-Lisp" "Lisp" "Clojure") :test 'equalp)
                  (call-interactively 'paredit-kill)
                (call-interactively 'kill-line)))))
(evil-mode 1)
;; * to search forward, # to search backward
(use-package evil-visualstar
  :after (evil)
  :init (progn
          (global-evil-visualstar-mode)
          (setq evil-visualstar/persistent nil)))

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
        (cond ((~file-pattern? text)
               (if (and (string-equal text (bowser:get-path-current-line))
                        (f-directory? text)
                        (f-exists? text))
                   (bowser:expand-or-collapse-dir)
                 (~smart-open-file text)))
              (t
               (wand:eval-string text)))))

    (defun* ~exec|-with-cp (text)
      "TODO"
      (interactive)
      (destructuring-bind (command . buffer)
          (if (boundp 'local/cp-buffer)
              (let ((cp-selection (with-current-buffer local/cp-buffer
                                    (~current-selection))))
                (if (or (string-empty-p cp-selection)
                        (string-equal cp-selection text))
                    (cons text (current-buffer))
                  (cons cp-selection (current-buffer))))
            (cons text (current-buffer)))
        (with-current-buffer buffer
          (~exec| command))))

    (defun* ~exec<-next-line-old (text)
      (interactive)
      (~open-line 1)
      (beginning-of-line)
      (~exec< text))

    (defun* ~exec<-next-line (text)
      (interactive)
      (~open-line 1)
      (beginning-of-line)
      (bs:exec text))

    (setq wand:*rules*
          (list (wand:create-rule :match (rx bol (0+ " ") "|")
                                  :capture :after
                                  :action #'~exec|
                                  ;; :action #'~exec|-with-cp
                                  )
                (wand:create-rule :match (rx bol (0+ " ") "<")
                                  :capture :after
                                  :action #'~exec<)
                (wand:create-rule :match (rx bol (0+ " ") "$")
                                  :capture :after
                                  :action #'~exec<-next-line)
                (wand:create-rule :match (rx bol (0+ " ") ">")
                                  :capture :after
                                  :action #'~exec>)
                (wand:create-rule :match (rx bol (0+ " ") "!!!")
                                  :capture :after
                                  :action #'~exec-with-term-emu)
                (wand:create-rule :match (rx bol (0+ " ") "!@")
                                  :capture :after
                                  :action #'~exec-with-term-emu-detach)
                (wand:create-rule :match (rx bol (0+ " ") "!!")
                                  :capture :after
                                  :action #'~exec-with-pause-in-term-emu)
                (wand:create-rule :match (rx bol (0+ " ") "!")
                                  :capture :after
                                  :action #'~exec-pop-up)
                (wand:create-rule :match "----\n[^ ]* +"
                                  :capture :after
                                  :action #'~current-snippet->file)
                (wand:create-rule :match (rx bol (0+ " ") "chrome:")
                                  :capture :after
                                  :action #'~open-with-google-chrome)
                (wand:create-rule :match (rx bol (0+ " ") "https?://")
                                  :capture :whole
                                  :action #'~web-browse-gui)
                (wand:create-rule :match ".*\\.html$"
                                  :capture :whole
                                  :action #'~web-browse-gui)
                (wand:create-rule :match (rx bol (0+ " ") "in:")
                                  :capture :after
                                  :action #'~wand:set-current-dir)
                (wand:create-rule :match (rx bol (0+ " ") "file:")
                                  :capture :after
                                  :action #'~smart-open-file)
                (wand:create-rule :match (rx (0+ (or any "\n")))
                                  :capture :whole
                                  :skip-comment t
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
