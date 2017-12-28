;;
;; Copyright (C) 2014-2017 Ha-Duong Nguyen (@cmpitg)
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
;; Vimdiff implementation - More intuitive than Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/justbur/emacs-vdiff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vdiff
  :ensure t
  :commands vdiff-files)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package epa-file
  :ensure epa
  :config (progn
            (epa-file-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/abo-abo/hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better way to switch to and swap windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/abo-abo/ace-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package switch-window
;;  :ensure t)

(use-package ace-window
  :init (progn
          (setq aw-dispatch-alway t)
          (setq aw-keys '(?a ?h ?t ?s ?g ?r ?p ?l ?, ?. ?p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/abo-abo/swiper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Smart completion - Because Helm is a clusterfuck
;;

(use-package counsel
  :config (progn
            (use-package ivy-hydra)
            (use-package ivy-anywhere)
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

            ;; Default matching
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
            (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
            ;; (require 'flx)
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

            (with-eval-after-load "ivy"
              (defun* ~ivy-next-line+ (&optional (n-lines 5))
                (interactive)
                (ivy-next-line n-lines))

              (defun* ~ivy-prev-line+ (&optional (n-lines 5))
                (interactive)
                (ivy-previous-line n-lines))

              (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

              ;; A Keybinding in minibuffer
              (bind-keys :map ivy-minibuffer-map
                         ("M-c" . ~ivy-prev-line+)
                         ("M-t" . ~ivy-next-line+)
                         ("C-m" . ivy-alt-done)
                         ("<f3>" . ivy-occur)
                         ("<s-return>" . ivy-dispatching-done)
                         ("<C-return>" . ivy-immediate-done)
                         ("<S-return>" . ivy-call)))))

;; ? How to perform some action
;; ? How to follow
;; ? In find-file - how to change directory
;; ? Integration with Company

;;
;; http://oremacs.com/swiper/
;;

(use-package counsel
  :ensure t
  :config (progn
            (ivy-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/boyw165/history
;; Very buggy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package history
;;   :ensure t
;;   :diminish history-mode
;;   :config (progn
;;             (setq history-history-max 256
;;                   history-window-local-history nil)
;;             (history-mode 1)

;;             (bind-key "s-h" 'history-preview-prev-history history-map)
;;             (bind-key "s-n" 'history-preview-next-history history-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporarily save & navigate between points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package point-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure t
  :commands er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live function signature at echo area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :diminish eldoc-mode
  :config (progn
            (eldoc-add-command 'paredit-backward-delete
                               'paredit-close-round)

            (add-hook 'emacs-lisp-mode-hook        'turn-on-eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook  'turn-on-eldoc-mode)
            (add-hook 'ielm-mode-hook              'turn-on-eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just to hide undo-tree mode from mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :diminish undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced file management with Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired+
  :ensure t
  :init (progn
          (setq dired-listing-switches "-lahF")
          ;; Reuse current buffer when opening file/dir
          (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired-details+
  :ensure t
  :init (progn
          (use-package dired-single
            :ensure t))
  :config (progn
            (setq dired-listing-switches "-lhFgG --group-directories-first")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save and restore current editing point when opening a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package saveplace
  :ensure t
  :init (progn
          (save-place-mode 1)
          (setq-default save-place t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package color-theme
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smoother scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smooth-scrolling
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom unique naming method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package uniquify
  :init (progn
          (setq uniquify-buffer-name-style 'forward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editable Ack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep-ack
  :ensure wgrep-ack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick jumping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :commands ace-jump-mode
  :config (progn
            (ace-jump-mode-enable-mark-sync)
            ;; * Without prefix, ace-jump directs toward char
            ;; * With 1 prefix, line
            ;; * With 2 prefixes, word
            (setq ace-jump-mode-submode-list
                  '(ace-jump-char-mode
                    ace-jump-line-mode
                    ace-jump-word-mode))))


;; ace-jump with buffer switching
(use-package ace-jump-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load before auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :diminish yas-minor-mode
  :config (progn
            (add-to-list 'yas-snippet-dirs (expand-file-name *snippet-dir*))
            (yas-global-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Company mode: https://github.com/company-mode/company-mode
;; Auto-complete mode: https://github.com/auto-complete/auto-complete
;;
;; Comparison:
;;
;; * https://github.com/company-mode/company-mode/issues/68
;;
;; * Company uses pop-tip, much more usable than pop-up.el which Auto-complete
;;   is using.
;;
;; * Simpler to setup, "just-work", less edge cases
;;
;; * Simpler configuration
;;
;; * Well-thought APIs, backends and frontends are easy to write and integrate
;;

;; (use-package auto-complete
;;   :diminish auto-complete-mode
;;   :ensure t
;;   :init (progn
;;           (require 'auto-complete-config)
;;           (ac-config-default)
;;           (setq ac-sources
;;                 '(ac-source-filename
;;                   ac-source-functions
;;                   ;; ac-source-yasnippet
;;                   ac-source-variables
;;                   ac-source-symbols
;;                   ac-source-features
;;                   ac-source-abbrev
;;                   ac-source-words-in-same-mode-buffers
;;                   ac-source-dictionary))

;;           (auto-complete-mode 1)
;;           (setq ac-fuzzy-enable t)

;;           (add-hook 'ruby-mode-hook
;;                     (lambda ()
;;                       (add-to-list 'ac-sources 'ac-source-rsense-method)
;;                       (add-to-list 'ac-sources 'ac-source-rsense-constant)))))

(use-package company
  :ensure t
  :diminish company-mode
  :config (progn
            (global-company-mode 1)

            (bind-key "C-/" 'company-complete company-mode-map)

            (use-package pos-tip
              :ensure t)

            (use-package company-quickhelp
              :ensure t
              :config (progn
                        (company-quickhelp-mode 1)

                        ;; Do not trigger automatically
                        (setq company-quickhelp-delay nil)
                        (bind-key "M-h" 'company-quickhelp-manual-begin
                                  company-active-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting phrase and expression when needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hi-lock
  :ensure t
  :commands (highlight-phrase highlight-regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-pairing brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens-config
  :config (progn
            (smartparens-global-mode)))

(use-package paredit
  :ensure t
  :config (progn
            (defadvice paredit-mode (around disable-otherparenslib-around (arg))
              "Disable autopairs mode if paredit-mode is turned on."
              ad-do-it
              (cond ((null ad-return-value)
                     (smartparens-mode 1))
                    (t
                     (smartparens-mode 0))))

            (ad-activate 'paredit-mode)

            ;; Use in minibuffer
            (defun conditionally-enable-paredit-mode ()
              "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
              (if (eq this-command 'eval-expression)
                  (paredit-mode 1)))

            (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

            ;; Use with SLIME REPL
            (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

            ;; Stop SLIME's REPL from grabbing DEL,
            ;; which is annoying when backspacing over a '('
            (defun override-slime-repl-bindings-with-paredit ()
              (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))

            (add-hook 'slime-repl-mode-hook
                      'override-slime-repl-bindings-with-paredit)

            (add-hook 'emacs-lisp-mode-hook       '~load-paredit-mode)
            (add-hook 'lisp-mode-hook             '~load-paredit-mode)
            (add-hook 'lisp-interaction-mode-hook '~load-paredit-mode)
            (add-hook 'scheme-mode-hook           '~load-paredit-mode)

            ;;
            ;; Helpers
            ;;

            (defun ~paredit-up-all ()
              (interactive)
              (ignore-errors
                (loop do (paredit-forward-up))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Adjust indentation based on current file
(use-package dtrt-indent
  :config (progn
            (dtrt-indent-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/syohex/emacs-emamux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emamux
  :commands (emamux:send-command emamux:send-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Remembering & jumping back and forth between window layouts
;; (configurations).  Ref: https://www.emacswiki.org/emacs/WinnerMode
;;
(use-package winner
  :init (progn
          (winner-mode 1)))

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
                        (add-hook 'clojure-mode-hook 'evil-paredit-mode)
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
            (with-eval-after-load "evil-vars"
              (evil-set-initial-state 'term-mode 'emacs)
              (evil-set-initial-state 'multi-term-mode 'emacs)
              (evil-set-initial-state 'ansi-term-mode 'emacs)
              (evil-set-initial-state 'magit-log-edit-mode 'emacs)
              (evil-set-initial-state 'magit-popup-mode 'emacs)
              (evil-set-initial-state 'magit-file-mode 'emacs)
              (evil-set-initial-state 'neotree-mode 'emacs)
              (evil-set-initial-state 'dired-mode 'emacs)
              (evil-set-initial-state 'nav-mode 'emacs)
              (evil-set-initial-state 'grep-mode 'emacs)
              (evil-set-initial-state 'bs-mode 'emacs)
              (evil-set-initial-state 'cider-repl-mode 'emacs)
              (evil-set-initial-state 'cider-popup-buffer-mode 'emacs)
              (evil-set-initial-state 'cider--debug-mode 'emacs)
              (evil-set-initial-state 'cider-temp-mode 'emacs)
              (evil-set-initial-state 'help-mode 'emacs)
              (evil-set-initial-state 'compilation-mode 'emacs)
              (evil-set-initial-state 'ivy-occur-mode 'emacs)
              (evil-set-initial-state 'ibuffer-mode 'normal))

            (setq evil-emacs-state-cursor 'bar)
            (setq-default cursor-type 'hbar)
            (setq-default cursor-type 'bar)

            ;; Better granularity for undo-tree
            (setq evil-want-fine-undo t)

            (define-key evil-insert-state-map "\C-y" 'yank)
            (define-key evil-insert-state-map "\C-o" '~open-line)
            (define-key evil-insert-state-map "\C-w" 'kill-region)
            (define-key evil-insert-state-map "\C-a" '~move-to-beginning-of-line)
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
                      ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern-based command execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wand
  :ensure t
  :config
  (progn
    (defun ~quick-action (text)
      "Executes special action."
      (interactive)
      (cond ((~file-pattern? text)
             (toolbox:open-file text))
            ((and (derived-mode-p 'emacs-lisp-mode)
                  (s-starts-with? "(" text))
             (~eval-string text))
            (t
             (wand:eval-string text))))

    (setq wand:*rules*
          (list (wand:create-rule :match "----\n[^ ]* +"
                                  :capture :after
                                  :action ~current-snippet->file)
                (wand:create-rule :match "\\$ "
                                  :capture :after
                                  :action ~popup-shell-command)
                (wand:create-rule :match ">\\$ "
                                  :capture :after
                                  :action erun)
                (wand:create-rule :match "> "
                                  :capture :after
                                  :action srun)
                (wand:create-rule :match "https?://"
                                  :capture :whole
                                  :action ~firefox)
                (wand:create-rule :match "web:"
                                  :capture :after
                                  :action (lambda (str)
                                            (interactive)
                                            (~firefox (format "file://%s" str))))
                (wand:create-rule :match "file:"
                                  :capture :after
                                  :action toolbox:open-file)
                (wand:create-rule :match "#> "
                                  :capture :after
                                  :action ~add-bracket-and-eval)
                (wand:create-rule :match "gg:"
                                  :capture :after
                                  :action ~google)
                (wand:create-rule :match "ff:"
                                  :capture :after
                                  :action ~send-to-mozrepl)
                (wand:create-rule :match "mailto:"
                                  :capture :after
                                  :action (lambda (str)
                                            (~send-mail :to str)))
                ;; (wand:create-rule :match "\"[^\"]*\""
                ;;                   :capture "\"\\([^\"]*\\)\""
                ;;                   :action (lambda (str)
                ;;                             (interactive)
                ;;                             (message-box "%s" str)
                ;;                             (toolbox:open-file str)))
                ;; (wand:create-rule :match "'[^']*'"
                ;;                   :capture "'\\([^']*\\)'"
                ;;                   :action toolbox:open-file)
                (wand:create-rule :match ".*\\.html$"
                                  :capture :whole
                                  :action (lambda (path)
                                            (interactive)
                                            (toolbox:open-with path "web-browser-gui %s")))
                (wand:create-rule :match (rx (0+ (or any "\n")))
                                  :capture :whole
                                  :skip-comment nil
                                  :action ~quick-action)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find file from X selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/asjo/fffs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  (defun cmpitg/fffs-load-path ()
    (~get-config "local-packages/fffs")))

(use-package find-file-from-selection
  :load-path (lambda () (list (cmpitg/fffs-load-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple scratch buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'multi-scratch)
(use-package multi-scratch
  :commands multi-scratch-new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhanced find-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice find-file (around find-files activate)
  "Also find all files within a list of files. This even works recursively."
  (if (listp filename)
      (loop for f in filename do (find-file f wildcards))
      ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading essential Emacs functionalities")
(provide 'ee:config-core-functionalities)
