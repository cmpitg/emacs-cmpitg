;;
;; Copyright (C) 2014-2016 Ha-Duong Nguyen (@cmpitg)
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
;; Must be in machine-specific config
;;

;; (setq user-full-name ""
;;       user-mail-address ""
;;       *toolbox-path* ""
;;       *saved-macro-path* ""
;;       *scratch-dir* ""
;;       *sbcl-bin-path* ""
;;       *quicklisp-path* ""
;;       *notes-path* "")

(setenv "XDG_DATA_DIRS" "/usr/share/i3:/usr/local/share:/usr/share")

(eval-after-load "vdiff-mode"
  '(progn
     (bind-key "<f6>" vdiff-mode-prefix-map vdiff-mode-map)
     (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)))

;; (~update-gpg-agent-info)


(setf openwith-associations
      '(("\\.pdf\\'" "evince" (file))
        ("\\.mp3\\'" "xmms" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "u smplayer" ("-idx" file))
        ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Q: What happens if I turn off all syntax coloring?
;;; A: Still works well, or even better
;;;
(global-font-lock-mode -1)
;; (global-font-lock-mode 1)

(add-hook 'adoc-mode-hook
          (lambda ()
            (font-lock-mode -1)))

(add-hook 'markdown-mode-hook
          (lambda ()
            (font-lock-mode -1)))

(use-package scss-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimental
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package sublimity
  :ensure t
  :config (progn
            (require 'sublimity-scroll)
            (sublimity-mode 1)))

(use-package persistent-scratch
  :ensure t
  :config (progn
            (persistent-scratch-setup-default)
            (persistent-scratch-autosave-mode 1)))

;; (global-auto-complete-mode -1)

(use-package restclient
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global keybindings should be centralized.  The need for better management
;; is greater than the need for falling back.
;;

;;
;; File navigation
;;

(bind-key "s-; f n" '~visit-my-notes)

(bind-key "C-<home>" '~jekyll-add-last-updated)

;; Remove this prefix key by any chance
(bind-key "s-SPC" 'nil)

(bind-key "<S-f12>" '~visit-toolbox)
(bind-key "C-<f1>" '~switch-to-scratch)
(bind-key "<S-f2>" '~switch-to-scratch-common-lisp)
(bind-key "<menu> <menu>" 'switch-window)
(bind-key "C-%" '~layout/default)
(bind-key "C-7" '~layout/vsplit)
(bind-key "C-5" '~layout/hsplit)
(bind-key "M-ESC"   '~keyboard-quit)
;; (bind-key "<f8>"    '~list-buffers)
(bind-key "<f8>"    'helm-mini)
(bind-key "s-\\" 'helm-semantic-or-imenu)
(bind-key "<C-tab>" '~switch-to-last-buffer)
(bind-key "C-@" 'slime-switch-to-output-buffer)

;; (bind-key "C-c <mouse-1>" 'kill-ring-save)
;; (bind-key "C-c <mouse-2>" 'kill-region)
;; (bind-key "C-c <mouse-3>" 'yank)

(bind-key "<s-return>"      'switch-to-eshell-back-and-forth)
(bind-key "<s-S-return>"    'cd-current-buffer-dir-and-switch-to-eshell)

(defun* ~neotree (&optional (dir "."))
  "Start NeoTree."
  (interactive)
  (neotree-dir dir)
  (call-interactively 'other-window))

(defun bind-spacemacs-like-keys ()
  "Bind keys inspired by Spacemacs."
  (interactive)

  ;; Movement
  (~bind-key-with-prefix "<" 'beginning-of-buffer)
  (~bind-key-with-prefix ">" 'end-of-buffer)
  (~bind-key-with-prefix "j" 'ace-jump-mode)
  (~bind-key-with-prefix "s-n" 'windmove-right)
  (~bind-key-with-prefix "s-h" 'windmove-left)
  (~bind-key-with-prefix "s-c" 'windmove-up)
  (~bind-key-with-prefix "s-t" 'windmove-down)
  (~bind-key-with-prefix "." 'dumb-jump-go)
  (~bind-key-with-prefix "," 'dumb-jump-back)
  (~bind-key-with-prefix ";" 'dumb-jump-quick-look)
  (~bind-key-with-prefix "\\" 'helm-semantic-or-imenu)

  ;; M-x
  (~bind-key-with-prefix "SPC" 'helm-M-x)
  (~bind-key-with-prefix "v p c" '~visit-experimental-config)
  (~bind-key-with-prefix "o p" '~open-project)
  (~bind-key-with-prefix "o f" '~open-file)

  ;; Buffer
  (~bind-key-with-prefix "s b" 'save-buffer)
  (~bind-key-with-prefix "l b" '~list-buffers)
  (~bind-key-with-prefix "r b" 'revert-buffer)
  (~bind-key-with-prefix "n n" '~new-buffer)
  (~bind-key-with-prefix "p p" 'popwin:messages)

  ;; File
  (~bind-key-with-prefix "f o" 'ido-find-file-other-window)
  (~bind-key-with-prefix "f b" '~file/browse)
  (~bind-key-with-prefix "f c p" '~file/copy-path)
  (~bind-key-with-prefix "f c d" '~file/copy-directory)
  (~bind-key-with-prefix "o a" '~neotree)

  (~bind-key-with-prefix "m f n" '~my/file-notes)

  ;; Project
  (~bind-key-with-prefix "p s b" 'helm-projectile-switch-to-buffer)

  ;; Window
  (~bind-key-with-prefix "w c" '~window/change)
  (~bind-key-with-prefix "o w" '~one-window)
  (~bind-key-with-prefix "w s v" '~window/split-vertically)
  (~bind-key-with-prefix "w s s" '~window/split-horizontally)

  ;; Git
  (~bind-key-with-prefix "g s" '~git/status)

  ;; Config
  (~bind-key-with-prefix "c a" '~visit-package-config)
  (~bind-key-with-prefix "c k" '~visit-keybindings)
  (~bind-key-with-prefix "c p" '~visit-experimental-config)
  (~bind-key-with-prefix "b k" '~bind-key-temporary)

  ;; Toolbox
  (~bind-key-with-prefix "t b" '~toolbox)
  (~bind-key-with-prefix "c l f" '~clone-file)
  (~bind-key-with-prefix "n f " 'new-frame)

  ;; Help
  (~bind-key-with-prefix "h k" '~help-key)
  (~bind-key-with-prefix "l k b" '~list-keybindings)

  ;; Google
  (~bind-key-with-prefix "g o o" '~google)

  ;; With Return key
  (bind-key "RET n" '~visit-my-notes evil-normal-state-map)
  (bind-key "RET g" 'magit-status evil-normal-state-map)
  (bind-key "C-z" 'keyboard-quit evil-normal-state-map)
  (bind-key "C-z" 'keyboard-quit evil-visual-state-map)
  (bind-key "C-z" 'keyboard-quit evil-motion-state-map)

  (add-hook 'lisp-mode-hook
            (lambda ()
              (~bind-key-with-prefix-local "h h" 'hyperspec-lookup)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (~bind-key-with-prefix-local "h f" '~emacs-lisp/help-function))))

(defalias '~my/file-notes '~visit-my-notes)

(defalias '~emacs-lisp/help-function 'describe-function)
(defalias '~help-key 'describe-key)
(defalias '~list-keybindings 'describe-personal-keybindings)

(defalias '~project/open 'helm-projectile-switch-project)
(defalias '~open-project 'helm-projectile-switch-project)
(defalias '~open-file    'find-file)

(defalias '~toolbox '~visit-toolbox)

(defalias '~window/change 'other-window)
(defalias '~window/split-vertically 'split-window-vertically)
(defalias '~window/split-horizontally 'split-window-horizontally)

(defalias '~visit-package-config '~visit-cmpitg-package-config)

(defalias '~git/status 'magit-status)
(defalias '~file/browse 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf magit-push-always-verify 'pp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* ~update-gpg-agent-info (&optional (env-file "~/.gnupg/gpg-agent.env"))
  "Updates GnuPG agent info and returns a list of `cons'es that
described the updated list."
  (interactive)
  (let ((content (~read-file env-file)))
    (-map (lambda (var=val)
            (let* ((elements (s-split "=" var=val))
                   (var      (first elements))
                   (val      (second elements)))
              (setenv var val)
              (cons var val)))
          (butlast (s-split "\n" content)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/dominikh/go-mode.el

(use-package go-mode-autoloads
  :ensure go-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-resizing wihh golden ratio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package golden-ratio
;;   :ensure golden-ratio
;;   :diminish ""
;;   :config (progn
;;             (golden-ratio-mode 1)
;;             (setq golden-ratio-exclude-mode '(" *NeoTree*" "ediff-mode" "dired-mode"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *emacs-menu-file*           "/m/Toolbox/Emacs-Menu.txt")

(defun ~is-default-layout? ()
  "Determine whether the current layout is my personal default."
  (and (eq 3 (count-windows))
       (get-buffer-window *neotree-buffer-name*)
       (get-buffer-window (f-filename *emacs-menu-file*))))

(defun ~layout/switch-to-main-buffer ()
  "Switch to main buffer if current layout is default layout."
  (interactive)
  (and (~is-default-layout?)
       (or (string-equal (buffer-name (current-buffer))
                         (f-filename *emacs-menu-file*))
           (s-starts-with? " *" (buffer-name (current-buffer)))
           (s-starts-with? "*"  (buffer-name (current-buffer))))
       (windmove-right)))

(defun* ~layout/default ()
  "Activate my personal default layout:
   * Left top: NeoTree, sticky
   * Left bottom: Acme-like, sticky, convenient menu: `/m/Toolbox/Emacs-Menu.txt'
   * Center: Main window"
  (interactive)
  (~one-window)
  (let ((menu-buffer-name (f-filename *emacs-menu-file*))
        (message-log-max  nil))
    (cond ((~is-default-layout?)
           (~layout/switch-to-main-buffer))

          (t
           ;; Kill NeoTree
           ;; (and (get-buffer *neotree-buffer-name*)
           ;;      (kill-buffer *neotree-buffer-name*))

           ;; Kill Menu
           ;; (and (get-buffer menu-buffer-name)
           ;;      (kill-buffer menu-buffer-name))

           ;; (progn
           ;;   ;; Start NeoTree
           ;;   (neotree-dir ".")
           ;;   (set-window-dedicated-p (get-buffer-window *neotree-buffer-name*) t)

           ;;   ;; Start Emacs menu
           ;;   (split-window-vertically)
           ;;   (windmove-down)
           ;;   (find-file *emacs-menu-file*)
           ;;   (text-scale-adjust -1)
           ;;   (set-window-dedicated-p (selected-window) t)

           ;;   (~layout/switch-to-main-buffer)
           ;;   (set-window-dedicated-p (selected-window) t))
           ))))

(defun* ~layout/hsplit ()
  "Activate my personal horizontal split layout."
  (interactive)
  (~layout/default)
  (split-window-horizontally)
  (set-window-dedicated-p (selected-window) nil))

(defun* ~layout/vsplit ()
  "Activate my personal vertically split layout."
  (interactive)
  (~layout/default)
  (split-window-vertically)
  (set-window-dedicated-p (selected-window) nil))

(defun ~neotree ()
  "Activate neotree and make the window sticky."
  (interactive)
  (call-interactively 'neotree)
  (set-window-dedicated-p (selected-window) t))

(defun ~clone-file ()
  "Clone current file into a buffer."
  (interactive)
  (copy-to-register 'clone
                    (save-excursion
                      (beginning-of-buffer)
                      (point))
                    (save-excursion
                      (end-of-buffer)
                      (point)))
  (call-interactively 'multi-scratch-new)
  (insert-register 'clone)
  (keyboard-quit))

(defun ~list-buffers ()
  (interactive)
  (helm-mini))

;;
;; helm-follow currently comes with a cost that it opens every file user is
;; selecting in the Helm buffer. Add this when you're absolutely sure you
;; would open all files when visiting them.
;;
;; You could C-c C-f to activate helm-follow-mode manually.
;;
;; (add-hook 'helm-after-update-hook #'(lambda ()
;;                                       (helm-follow-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido everywhere
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ido is replaced completely with Helm
;;

;; (use-package ido-ubiquitous
;;   :ensure ido-ubiquitous
;;   :config (progn
;;             (setq ido-everywhere t)))

;; (use-package ido
;;   :config (progn
;;             (bind-key "s-t" 'ido-next-match minibuffer-local-map)
;;             (bind-key "s-c" 'ido-prev-match minibuffer-local-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/tjarvstrand/edts
;;   edts-man-setup to setup documentation
;;   Make sure you have `rebar' 2.0.1+ installed, then
;;       cd ~/.emacs.d/elpa/edts*/
;;       find . -name rebar -print0 | xargs -0 -I{} echo "{}"
;;       make

;; (use-package edts-start
;;   :ensure edts
;;   :config (progn
;;             (setq erlang-electric-commands '(erlang-electric-comma
;;                                              erlang-electric-semicolon))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package alchemist
;;   :ensure alchemist
;;   :init (progn
;;           (~auto-load-mode '("\\.ex$" "\\.exs$") 'alchemist-mode)

;;           ;; Don't ask to save changes before running tests
;;           (setq alchemist-test-ask-about-save nil)

;;           ;; Display compilation output
;;           (setq alchemist-test-display-compilation-output t)))

(use-package elixir-mode
  :ensure t)

;; (use-package erlang-start
;;   :ensure erlang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs support with company mode in Jedi is weak
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use autocomplete mode
;; (use-package jedi
;;   :ensure t
;;   :config (progn
;;             (setq jedi:setup-keys nil)
;;             (setq jedi:tooltip-method nil)
;;             (setq jedi:complete-on-dot t)

;;             (autoload 'jedi:setup "jedi" nil t)
;;             (add-hook 'python-mode-hook 'jedi:setup)

;;             (defvar jedi:goto-stack '())

;;             (defun jedi:jump-to-definition ()
;;               (interactive)
;;               (add-to-list 'jedi:goto-stack
;;                            (list (buffer-name) (point)))
;;               (jedi:goto-definition))

;;             (defun jedi:jump-back ()
;;               (interactive)
;;               (let ((p (pop jedi:goto-stack)))
;;                 (if p (progn
;;                         (switch-to-buffer (nth 0 p))
;;                         (goto-char (nth 1 p))))))

;;             (defun my/python-mode-hook ()
;;               (add-to-list 'company-backends 'company-jedi))

;;             (add-hook 'python-mode-hook 'my/python-mode-hook)

;;             ;; (add-hook 'python-mode-hook
;;             ;;           '(lambda ()
;;             ;;              (local-set-key (kbd "C-.") 'jedi:jump-to-definition)
;;             ;;              (local-set-key (kbd "C-,") 'jedi:jump-back)
;;             ;;              (local-set-key (kbd "C-c d") 'jedi:show-doc)
;;             ;;              (local-set-key (kbd "C-<tab>") 'jedi:complete)))
;;             ))

;; Use company-mode
;; (use-package company-jedi
;;   :ensure t
;;   :config (progn
;;             (defvar jedi:goto-stack '())

;;             (defun jedi:jump-to-definition ()
;;               (interactive)
;;               (add-to-list 'jedi:goto-stack
;;                            (list (buffer-name) (point)))
;;               (jedi:goto-definition))

;;             (defun jedi:jump-back ()
;;               (interactive)
;;               (let ((p (pop jedi:goto-stack)))
;;                 (if p (progn
;;                         (switch-to-buffer (nth 0 p))
;;                         (goto-char (nth 1 p))))))

;;             (defun my/python-mode-hook ()
;;               (add-to-list 'company-backends 'company-jedi))

;;             (add-hook 'python-mode-hook 'my/python-mode-hook)))

;; (use-package django-mode
;;   :ensure t
;;   :commands python-mode
;;   :config (progn
;;             (use-package python-django
;;               :ensure python-django)
;;             (use-package django-html-mode
;;               :mode "\\\\.djhtml$")))

;;
;; Elpy seems to be a very strong contender
;;
;; http://elpy.readthedocs.io/en/latest/index.html
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

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :ensure elpy
  :init (progn
          (elpy-enable)

          ;; I don't want to highlight indentation
          (setq elpy-modules (remove 'elpy-module-highlight-indentation
                                     elpy-modules))

          (setq elpy-rpc-backend "jedi")

          (defvar my/python-goto-stack (list))

          (defun my/python-jump-to-definition ()
            (interactive)
            (add-to-list 'my/python-goto-stack
                         (list (buffer-name) (point)))
            (elpy-goto-definition))

          (defun my/python-jump-back ()
            (interactive)
            (let ((p (pop my/python-goto-stack)))
              (if p (progn
                      (switch-to-buffer (nth 0 p))
                      (goto-char (nth 1 p))))))

          (defun my/elpy-mode-hook ()
            (bind-key "C-c ." 'my/python-jump-to-definition)
            (bind-key "C-c ," 'my/python-jump-back))

          (add-hook 'python-mode-hook 'my/elpy-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guide key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Echo key sequence, step by step.
;;

;; (use-package guide-key
;;   :ensure guide-key
;;   :diminish guide-key-mode
;;   :config (progn
;;             (setq guide-key/guide-key-sequence '("C-x"
;;                                                  "C-c"
;;                                                  "C-h"
;;                                                  "s-;"
;;                                                  "SPC")
;;                   guide-key/recursive-key-sequence-flag t)
;;             (guide-key-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq *slime-quicklisp-path*
;;       (->> (directory-files "/m/opt/quicklisp-sbcl/dists/quicklisp/software/" nil "^slime.*")
;;         first
;;         (s-concat "/m/opt/quicklisp-sbcl/dists/quicklisp/software/"))
;;       *slime-quicklisp* (s-concat *slime-quicklisp-path* "/slime.el"))

;; (~add-load-path *slime-quicklisp-path*)

;; (load-file *slime-quicklisp*)

;; Better if indentation
(put 'if 'common-lisp-indent-function 2)

;; (use-package sly
;;   :load-path "/m/src/sly"
;;   :commands common-lisp-mode
;;   :config (progn
;;             (require 'sly-autoloads)
;;             ;; (add-hook 'inferior-lisp-mode-hook
;;             ;;           (lambda ()
;;             ;;             (inferior-sly-mode t)
;;             ;;             (~load-paredit-mode)
;;             ;;             (eldoc-mode 1)))

;;             ;; (add-hook 'sly-mode-hook
;;             ;;           (lambda ()
;;             ;;             (auto-complete-mode -1)))

;;             (font-lock-add-keywords 'lisp-mode
;;                                     '(("->" . font-lock-keyword-face)
;;                                       ("->>" . font-lock-keyword-face)))

;;             (setq inferior-lisp-program "~/bin/sbcl-cmpitg"
;;                   sly-complete-symbol-function 'sly-fuzzy-complete-symbol)

;;             ;; (setenv "XDG_DATA_DIRS" "/usr/share/i3:/usr/local/share:/usr/share")

;;             (load (expand-file-name "/m/opt/quicklisp/slime-helper.el"))
;;             (load "/m/opt/quicklisp/clhs-use-local.el" t)))

(use-package slime
  :ensure slime
  :commands common-lisp-mode
  :config (progn
            (put 'if 'common-lisp-indent-function 2)

            (add-hook 'inferior-lisp-mode-hook
                      (lambda ()
                        (inferior-slime-mode t)
                        (~load-paredit-mode)
                        (eldoc-mode 1)))

            (eval-after-load 'slime
              '(progn
                 (put 'if 'common-lisp-indent-function 2)))

            (font-lock-add-keywords 'lisp-mode
                                    '(("->" . font-lock-keyword-face)
                                      ("->>" . font-lock-keyword-face)))

            (setq inferior-lisp-program *sbcl-bin-path*
                  slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

            ;; https://github.com/anwyn/slime-company
            (eval-after-load 'company
              '(progn
                 (use-package slime-company
                   :ensure t)

                 (slime-setup '(slime-fancy slime-company))
                 ;; (bind-key "C-n" 'company-select-next company-active-map)
                 ;; (bind-key "C-p" 'company-select-previous company-active-map)
                 ;; (bind-key "C-d" 'company-show-doc-buffer company-active-map)
                 ;; (bind-key "M-." 'company-show-location company-active-map)
                 ))

            (load (concat *quicklisp-path* "slime-helper.el"))
            (load (concat *quicklisp-path* "clhs-use-local.el") t)))

(defun ~slime-connect-default ()
  "Connects to default Slime process."
  (interactive)
  (slime-connect "localhost" 4005))

(defun ~line-match? (regexp)
  "Determines of the current line matchs a regular expression."
  (s-matches? regexp (thing-at-point 'line)))

(defun ~select-sexp ()
  "Binds double click mouse-1 to select current sexp."
  (interactive)
  (cond ((eq ?\( (char-after))
         (mark-sexp))
        ((or (~line-match? (format "^ *%s+" comment-start))
             (~line-match? "^ +$"))
         (mark-line))
        (t
         (paredit-backward-up)
         (mark-sexp))))

(put 'if 'common-lisp-indent-function 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~paredit-up-all ()
  (interactive)
  (ignore-errors
    (loop do (paredit-forward-up))))

(defun ~emacs-lisp/eval-then-next ()
  "Evaluates current expression and move to the next line."
  (interactive)
  (ignore-errors
    (call-interactively '~paredit-up-all)
    (call-interactively 'eval-last-sexp)
    (next-line)))

(defun ~cl/eval-open ()
  "Evaluates current expression with Slime and move to the next
line."
  (interactive)
  (ignore-errors
    (call-interactively '~paredit-up-all)
    (call-interactively 'slime-eval-last-expression)
    (next-line)))

(bind-key "<C-return>" '~emacs-lisp/eval-then-next emacs-lisp-mode-map)

;; (eval-after-load "slime-mode"
;;   (bind-key "<C-return>" '~cl/eval-open slime-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure evil
  :config (progn
            (use-package evil-paredit
              :ensure evil-paredit
              :config (progn
                        (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
                        (add-hook 'lisp-mode-hook 'evil-paredit-mode)
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
            (eval-after-load 'evil-vars
              '(progn
                 (evil-set-initial-state 'term-mode 'emacs)
                 (evil-set-initial-state 'multi-term-mode 'emacs)
                 (evil-set-initial-state 'ansi-term-mode 'emacs)
                 (evil-set-initial-state 'magit-log-edit-mode 'emacs)
                 (evil-set-initial-state 'dired-mode 'emacs)
                 (evil-set-initial-state 'nav-mode 'emacs)
                 (evil-set-initial-state 'grep-mode 'emacs)
                 (evil-set-initial-state 'bs-mode 'emacs)
                 (evil-set-initial-state 'ibuffer-mode 'normal)))

            (setq evil-emacs-state-cursor 'bar)
            (setq-default cursor-type 'hbar)
            (setq-default cursor-type 'bar)

            (bind-spacemacs-like-keys)

            (define-key evil-insert-state-map "\C-y" 'yank)
            (define-key evil-insert-state-map "\C-o" '~open-line)
            (define-key evil-insert-state-map "\C-w" 'kill-region)
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
                      ))

            ;; Better jumper with C-i and C-o
            (use-package evil-jumper
              :ensure t
              :init (progn
                      (evil-jumper-mode t)))

            ;; Doesn't work
            ;; (evil-set-initial-state 'lisp-mode 'emacs)
            ;; (evil-set-initial-state 'emacs-lisp-mode 'emacs)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better Lisp indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/IndentingLisp
;;

;; (put 'if 'common-lisp-indent-function 1)
;; (put 'if 'common-lisp-indent-function 2)
;; (setq lisp-indent-function 'common-lisp-indent-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package multi-term
;;   :ensure t
;;   :config (progn
;;             (setq multi-term-program "/bin/zsh")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mag-menu
  :ensure mag-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Google this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package google-this
  :ensure t
  :commands google-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better package management interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package paradox
;;   :ensure paradox
;;   :bind ("s-v" . paradox-list-packages))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-jump buffer switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-buffer
  :ensure ace-jump-buffer
  :init (progn
          (bind-key "s-j" 'ace-jump-buffer)
          (bind-key "s-J" 'ace-jump-buffer-other-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; QML mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package qml-mode
;;   :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dockerfile mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode
  :mode "Dockerfile\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple menu building
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package simple-menu
  :load-path "/m/src/simple-menu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package js2-mode
;;   :ensure js2-mode
;;   :init (progn
;;           (~auto-load-mode "\\.js" 'js2-mode)))

;; tr 'A-Za-z' 'N-ZA-Mn-za-m'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pattern-based command execution
;;

(require 'wand)

(use-package wand
  :init (progn
          ;; TODO

          ;;
          ;; * Window management, switch to convenient window
          ;; * Tryout Twitter Bootstrap
          ;; * SCM
          ;; * Bookmark, open file in external programs
          ;; * Controlling Firefox, evaluation JavaScript
          ;; * Live shell command
          ;; * Slide show
          ;;
          ;; Some cool Acme use cases
          ;;

          ;;
          ;; $ ls
          ;; $ cd ~; ls
          ;; file:/mnt/Data/Knowledge/IT/Programming Languages/Common Lisp/On Lisp.pdf
          ;; file:/mnt/Data/
          ;; https://google.com
          ;; http://vnexpress.net
          ;; #> message-box "Hello World!!!!"
          ;; mail:cmpitg@gmail.com
          ;; mail/subscribe:<listname>@lists.debian.org -> <listname>-REQUEST@lists.debian.org
          ;; #> google "hello world program"
          ;;

          ;; (bind-key "<mouse-3>" 'wand:execute)

          ;; gg:español
          ;; ff:alert(document.title);
          ;; #> ~firefox "https://github.com/cmpitg/wand"
          ;; #> ~send-to-mozrepl "alert(document.title);"

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
                      (wand:create-rule :match ".*"
                                        :capture :whole
                                        :action (lambda (str)
                                                  (interactive)
                                                  ;; (message-box "Here: %s -> " str (~file-pattern? str))
                                                  (cond ((~file-pattern? str)
                                                         (multiple-value-bind (path pattern)
                                                             (~deconstruct-path str)
                                                           (find-file path)
                                                           (when pattern
                                                             (cond ((numberp pattern)
                                                                    (goto-line pattern))
                                                                   (t
                                                                    (beginning-of-buffer)
                                                                    (re-search-forward pattern))))))
                                                        ((and (eq 'lisp-mode major-mode)
                                                              (slime-connected-p))
                                                         ;; (message-box "lisp-mode")
                                                         (call-interactively 'slime-eval-region))
                                                        (t
                                                         ;; (message-box "emacs-lisp mode")
                                                         (call-interactively 'wand:eval-string)))))))))

(defun ~file-pattern? (str)
  "Determines if a string is a file pattern \(`path' or
`path:line-number', or `path:pattern'\)."
  (let ((str (s-trim str)))
   (or (f-exists? str)
       (let ((components (s-split ":" str)))
         (and (= 2 (length components))
              (f-exists? (first components)))))))

(defun ~deconstruct-path (path)
  "Deconstructs a file pattern into `\(values path
line-number\)', `\(values path pattern\)' or just `\(values
path\)' if there is no line number."
  (let ((path (s-trim path)))
    (if (~file-pattern? path)
        (or (and (f-exists? path)
                 (values path))
            (let ((components (s-split ":" path)))
              (values (first components) (let ((num (string-to-int (second components))))
                                           (if (zerop num)
                                               (second components)
                                             num)))))
      (values))))

(defun ~current-snippet->file (path)
  "Generates the corresponding file from current snippet."
  (interactive)
  (save-excursion
    (search-backward "[source")
    (next-line)
    (next-line)
    (beginning-of-line)
    (let ((start (point)))
      (search-forward "----")
      (previous-line)
      (end-of-line)
      (~write-to-file path (buffer-substring start (point))))
    (message "Written to %s" path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :config (progn
            (setq python-shell-interpreter "ipython"
                  python-shell-interpreter-args ""
                  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                  python-shell-completion-setup-code
                  "from IPython.core.completerlib import module_completion"
                  python-shell-completion-module-string-code
                  "';'.join(module_completion('''%s'''))\n"
                  python-shell-completion-string-code
                  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'pi 'package-install)
(defalias 'fl 'find-library)
(defalias 'll '~load-files)
(defalias 'ppath '~get-library-full-path)
(defalias 'sho 'goo-url-shorten)
(defalias 'erun '~execute-command-and-popup-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice find-file (around find-files activate)
  "Also find all files within a list of files. This even works recursively."
  (if (listp filename)
      (loop for f in filename do (find-file f wildcards))
      ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better IPC with emnode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *emnode-routes*
      '(("^.*//eval/?"         . ~ipc-eval)
        ("^.*//open/\\(.*\\)"  . ~ipc-open-file)
        ("^.*//exec/\\(.*\\)"  . ~ipc-exec-file)))

(defun ~ipc-eval (httpcon)
  (let* ((expr (format "%s" (emnode:http-data httpcon))))
    (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
    ;; (emnode:http-end httpcon (format "> Eval'ing: %s" expr))
    (unless (~string-empty? (s-trim expr))
      (emnode:http-end httpcon (format "%s" (~add-bracket-and-eval expr))))))

(defun ~ipc-open-file (httpcon)
  (let ((path (emnode:http-get-arg httpcon 1)))
    (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
    (emnode:http-end httpcon (format "> Opening: %s\n" path))
    (find-file path)))

(defun ~ipc-exec-file (httpcon)
  (let ((path (emnode:http-get-arg httpcon 1)))
    (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
    (emnode:http-end httpcon (format "> Executing: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (eval-buffer))))

;; $ curl 0:9999/eval/ -d 'message-box "Hello World"'
;; $ curl 0:9999/eval -d '(message-box "Hello") (message-box "World")'
;; $ curl 0:9999/open//m/src
;; $ curl 0:9999/exec//tmp/tmp.el

(use-package emnode
  :load-path "/m/src/emnode"
  :ensure elnode
  :config
  (progn
    (setq emnode:*log-level* emnode:+log-none+)
    (emnode:stop 9999)
    (ignore-errors
      ;; (emnode-start (lambda (httpcon)
      ;;                 (emnode-hostpath-dispatcher httpcon *emnode-routes*))
      ;;               :port 9999)
      (emnode:start-server *emnode-routes* :port 9999))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~jsify-var (str)
  "JavaScript-ifies a module name by CaMElizing it and stripping
extension and basename.

E.g.

\(~jsify-var \"hello\"\)
\(~jsify-var \"hello-world\"\)
\(~jsify-var \"../hello-world\"\)
\(~jsify-var \"../../hello-world\"\)
\(~jsify-var \"../aoeu/hello-world\"\)
\(~jsify-var \"../aoeu/hello-world.js\"\)
\(~jsify-var \"../aoeu/hello world.js\"\)"
  (first (-reduce-from (lambda (hold item)
                         (let* ((res (first hold))
                                (needs-cap (second hold))
                                (next-item (if needs-cap
                                               (capitalize item)
                                             item)))
                           (if (string-equal "-" item)
                               (list res t)
                             (list (format "%s%s" res next-item)
                                   nil))))
                       (list "" nil)
                       (-> (split-string str "/")
                           (-last-item)
                           (split-string "")))))

;; (~jsify-var "hello")
;; (~jsify-var "hello-world")
;; (~jsify-var "../hello-world")
;; (~jsify-var "../../hello-world")
;; (~jsify-var "../aoeu/hello-world")
;; (~jsify-var "../aoeu/hello-world.js")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package racket-mode
  :load-path "/m/src/racket-mode/"
  :ensure t
  :commands racket-mode
  :mode "\\.rkt\\'"
  :config (progn
            ;; TODO: Document about indent changing and keywording
            (dolist (sym '(λ
                           ~>
                           ~>>
                           define-values
                           get
                           post
                           put
                           patch
                           delete
                           call-with-parameterization
                           module+))
              (put sym 'racket-indent-function 1)
              (add-to-list 'racket-keywords (~symbol->string sym))
              ;; (add-to-list 'racket-builtins (~symbol->string sym))
              )

            (dolist (sym '(module
                           module*))
              (put sym 'racket-indent-function 2)
              (add-to-list 'racket-keywords (~symbol->string sym))
              ;; (add-to-list 'racket-builtins (~symbol->string sym))
              )

            (add-hook 'racket-mode-hook       '~load-paredit-mode)
            (add-hook 'racket-repl-mode-hook  '~load-paredit-mode)
            ;; (add-hook 'racket-mode-hook       'auto-complete-mode)
            ;; (add-hook 'racket-repl-mode-hook  'auto-complete-mode)

            (bind-key "C-c C-\\" '(lambda (prefix)
                                    (interactive "P")
                                    (if prefix
                                        (progn (insert "(λ () )")
                                               (backward-char))
                                      (insert "λ")))
                      racket-mode-map)

            (bind-key "C-c C-b" 'racket-run racket-mode-map)
            (bind-key "C-c C-z" 'other-window racket-repl-mode-map)
            ;; (bind-key "C-c C-z" (lambda ()
            ;;                       (interactive)
            ;;                       (call-interactively 'racket-repl)
            ;;                       (call-interactively 'other-window))
            ;;           racket-mode-map)
            (bind-key "C-M-x" 'racket-send-definition racket-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* ~visit-my-notes ()
  "Visits my notes."
  (interactive)
  (~helm-projectile-find-files-at-dir (or *notes-path*
                                          "~/Docs/Notes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Enhance narrow to region in ASCIIDoc mode.
;; Doesn't support multiple narrowing.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defvar *saved-major-mode-for-narrow* (ht))
(defvar *language->mode-symbol* (ht ('racket-mode  'scheme-mode)))
(defvar *narrowed-spaces* nil)

(defun ~widen-narrowed-region ()
  "Widen the narrowed region and reset major mode by saving and
reverting the buffer."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (loop until (eobp)
       do
         (beginning-of-line)
         (unless (~current-line-empty?)
           (insert *narrowed-spaces*))
         (forward-line 1)))
  (setq *narrowed-spaces* nil)          ; Not narrowed anymore
  (widen)
  (call-interactively (ht-get *saved-major-mode-for-narrow* (buffer-file-name)))
  (recenter-top-bottom))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
      (switch-to-buffer buf)))

(defun ~narrow-to-code-region-markdown ()
  "Narrow to code region in Markdown mode.  Reindent."
  (interactive)
  (save-excursion
    (let* ((begin-code-region (rx (0+ " ")
                                  "```"
                                  (0+ (any alnum "-_/"))
                                  eol))
           (end-code-region (rx (0+ " ")
                                "```"
                                eol))
           (language (save-excursion
                       (re-search-backward begin-code-region)
                       (let* ((line (thing-at-point 'line)))
                         (if (string-match (rx (0+ " ")
                                               "```"
                                               (group (1+ (any alnum "-_/")))
                                               eol)
                                           line)
                             (match-string 1 line)
                             "text"))))
           (begin-point (save-excursion
                          (re-search-backward begin-code-region)
                          (next-line)
                          (beginning-of-line)
                          (point)))
           (n-spaces (save-excursion
                       (re-search-forward begin-code-region)
                       (let* ((line (thing-at-point 'line))
                              (spaces (progn
                                        (string-match (rx bol (group (0+ " ")))
                                                      line)
                                        (match-string 1 line))))
                         (length spaces))))
           (end-point (save-excursion
                        (re-search-forward end-code-region)
                        (previous-line)
                        (end-of-line)
                        (point))))
      (ht-set! *saved-major-mode-for-narrow* (buffer-file-name) major-mode)
      (narrow-to-region begin-point end-point)
      (setq *narrowed-spaces* (~make-spaces n-spaces))

      ;; Remove all extra spaces
      (save-excursion
        (beginning-of-buffer)
        (loop until (eobp)
           do
             (unless (~current-line-empty?)
               (delete-forward-char n-spaces))
             (forward-line 1)))

      ;; (message-box "Right here!")
      (let* ((language-mode-symbol (intern (s-concat language "-mode")))
             (mode-symbol          (cond ((~is-function-defined? language-mode-symbol)
                                          language-mode-symbol)
                                         (t
                                          (ht-get *language->mode-symbol*
                                                  language-mode-symbol
                                                  'markdown-mode)))))
        ;; (message-box "Found: %s; Mode: %s"
        ;;              language-mode-symbol
        ;;              mode-symbol)
        (funcall mode-symbol)))))

(defun ~narrow-to-code-region-asciidoc ()
  "Narrow to code region in Markdown mode.  Reindent."
  (interactive)
  (save-excursion
    (let* ((begin-code-region (rx bol "----" eol))
           (end-code-region (rx bol "----" eol))
           (language (save-excursion
                       (re-search-backward begin-code-region)
                       (previous-line)
                       (let* ((line (trim-spaces (thing-at-point 'line))))
                         (if (equalp "[source]" line)
                             "text"
                           (-> (split-string line "\\[source,")
                               second
                               (split-string ",")
                               first
                               (split-string "\\]")
                               first)))))
           (begin-point (save-excursion
                          (re-search-backward begin-code-region)
                          (next-line)
                          (beginning-of-line)
                          (point)))
           (n-spaces (save-excursion
                       (re-search-forward begin-code-region)
                       (let* ((line (thing-at-point 'line))
                              (spaces (progn
                                        (string-match (rx bol (group (0+ " ")))
                                                      line)
                                        (match-string 1 line))))
                         (length spaces))))
           (end-point (save-excursion
                        (re-search-forward end-code-region)
                        (previous-line)
                        (end-of-line)
                        (point))))
      (ht-set! *saved-major-mode-for-narrow* (buffer-file-name) major-mode)
      (narrow-to-region begin-point end-point)
      (setq *narrowed-spaces* (~make-spaces n-spaces))

      ;; Remove all extra spaces
      (save-excursion
        (beginning-of-buffer)
        (loop until (eobp)
              do
              (unless (~current-line-empty?)
                (delete-forward-char n-spaces))
              (forward-line 1)))

      ;; (message-box "Right here!")
      (let* ((language-mode-symbol (intern (s-concat language "-mode")))
             (mode-symbol          (cond ((~is-function-defined? language-mode-symbol)
                                          language-mode-symbol)
                                         (t
                                          (ht-get *language->mode-symbol*
                                                  language-mode-symbol
                                                  'markdown-mode)))))
        ;; (message-box "Found: %s; Mode: %s"
        ;;              language-mode-symbol
        ;;              mode-symbol)
        (funcall mode-symbol)))))

(defalias '~narrow-to-code-region '~narrow-to-code-region-asciidoc)

(defun ~make-spaces (times)
  "Return a string of `times' spaces."
  (s-join "" (loop for i upto (1- times) collect " ")))

(defun ~is-narrowed? ()
  "Checks if the buffer is still narrowed by `~narrow-to-code-region'."
  (not (null *narrowed-spaces*)))

;; (ht-get *language->mode-symbol* 'racket 'markdown-mode)
;; (ht-get *language->mode-symbol* "racket")
;; (~is-function-defined? (intern "python-mode"))
;; (~is-function-defined? 'python-mode)

(defun ~current-line-empty? ()
  "Determine if current line contains only whitespaces."
  (string-match-p (rx bol
                      (0+ whitespace)
                      eol)
                  (first (s-split "\n" (thing-at-point 'line)))))

(defun ~toggle-narrow-to-code-region ()
  "Toggle narrowing to region in Markdown mode by calling
`~narrow-to-code-region' or `~widen-narrowed-region'
respectively."
  (interactive)
  (cond ((null *narrowed-spaces*)
         (~narrow-to-code-region))
        (t
         (~widen-narrowed-region))))

(defun lp:save-snippet ()
  "Extract code snippets to a file.  To be called upon Markdown
code block."
  (interactive)
  (save-excursion
    (let* ((file-path (lp:extract-file-path))
           (begin-point (save-excursion
                          (cond ((~is-narrowed?)
                                 (beginning-of-buffer)
                                 (point))
                                (t
                                 (re-search-backward "```")
                                 (next-line)
                                 (beginning-of-line)
                                 (point)))))
           (end-point   (save-excursion
                          (cond ((~is-narrowed?)
                                 (end-of-buffer)
                                 (point))
                                (t
                                 (re-search-forward "```")
                                 (previous-line)
                                 (end-of-line)
                                 (point)))))
           (content     (buffer-substring begin-point end-point)))
      (~write-to-file file-path content)
      (message "Saved to %s" file-path))))

(defun lp:extract-file-path ()
  "Extract file path from the first comment line."
  (save-excursion
    (let* ((raw-line (cond ((~is-narrowed?)
                            (beginning-of-buffer)
                            (thing-at-point 'line))
                           (t
                            (re-search-backward "```")
                            (next-line)
                            (thing-at-point 'line))))
           (line     (lp:remove-from-last-space raw-line)))
      (with-temp-buffer
        (insert line)
        (beginning-of-buffer)
        (cond ((or (ignore-errors (re-search-forward "%%path "))
                   (ignore-errors (re-search-forward "%%file ")))
               (let ((begin-point (point))
                     (end-point   (progn
                                    (end-of-line)
                                    (point))))
                 (buffer-substring begin-point end-point)))

              ((or (ignore-errors (re-search-forward "; "))
                   (ignore-errors (re-search-forward "# "))
                   (ignore-errors (re-search-forward "// "))
                   (ignore-errors (re-search-forward comment-start)))
               (let ((begin-point (point))
                     (end-point   (progn
                                    (end-of-line)
                                    (point))))
                 (buffer-substring begin-point end-point)))
              (t
               nil))))))

(defun lp:remove-from-last-space (str)
  ""
  str)

(defun lp:visit-snippet ()
  "Visit the snippet defined in current code block."
  (interactive)
  (let ((file-path (lp:extract-file-path)))
    (find-file file-path)))

;; Narrow to region and set mode
;; Widen and reset mode

;; (bind-key "s-; r l" '~racket-lookup-documentation)
(bind-key "s-; n n" '~toggle-narrow-to-code-region)
(bind-key "s-; n r" '~narrow-to-code-region)
(bind-key "s-; n w" '~widen-narrowed-region)

(bind-key "s-; s s" 'lp:save-snippet)
(bind-key "s-; s v" 'lp:visit-snippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common Lisp-style indentation
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'lisp-indent-function)
;;                  'common-lisp-indent-function)))
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'lisp-indent-function)
;;                  'common-lisp-indent-function)))

;; (remove-menu [menu-bar layout])

;; (make-submenu [menu-bar layout]
;;               :title "Layout")

;; (make-menu-item [menu-bar layout default]
;;                 :title "Default"
;;                 :action '~layout/default)

;; (make-menu-item [menu-bar layout hsplit]
;;                 :title "Horizontal split"
;;                 :action '~layout/hsplit)

;; (make-menu-item [menu-bar layout vsplit]
;;                 :title "Vertical split"
;;                 :action '~layout/vsplit)

;; (remove-menu [menu-bar literate])

;; (make-submenu [menu-bar literate]
;;               :title "Literate")

;; (make-menu-item [menu-bar literate toggle-narrow-to-region]
;;                 :title "Toggle narrowing to region"
;;                 :tooltip "Toogle narrowing to code region in Markdown mode."
;;                 :action '~toggle-narrow-to-code-region)
;; (make-menu-item [menu-bar literate narrow-to-region]
;;                 :title "Narrow to region"
;;                 :tooltip "Narrow to code region in Markdown mode."
;;                 :action '~narrow-to-code-region)
;; (make-menu-item [menu-bar literate widen-region]
;;                 :title "Widen region"
;;                 :tooltip "Widen the narrowed region, save file, an reload the buffer."
;;                 :action '~widen-narrowed-region)

;; (make-menu-separator [menu-bar literate separator] :after 'widen-region)
;; (make-menu-item [menu-bar literate generate-src]
;;                 :title "Generate source"
;;                 :tooltip "Generate source code."
;;                 :action 'ulqui:generate-src)
;; (make-menu-item [menu-bar literate generate-src-current-dir]
;;                 :title "Generate source (current dir)"
;;                 :tooltip "Generate source code from and to current directory."
;;                 :action 'ulqui:generate-src-current-dir)
;; (make-menu-item [menu-bar literate generate-html]
;;                 :title "Generate HTML"
;;                 :tooltip "Generate HTML."
;;                 :action 'ulqui:generate-html)
;; (make-menu-item [menu-bar literate generate-html-current-dir]
;;                 :title "Generate HTML (current dir)"
;;                 :tooltip "Generate HTML from and to current directory."
;;                 :action 'ulqui:generate-html-current-dir)
;; (make-menu-item [menu-bar literate generate-all]
;;                 :title "Generate all"
;;                 :tooltip "Generate source and HTML."
;;                 :action 'ulqui:generate-all)

;; (make-menu-separator [menu-bar literate seperator-2])
;; (make-menu-item [menu-bar literate srun-make]
;;                 :title "make (tmux)"
;;                 :tooltip "Run make with Tmux"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (srun "make")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu/Toolbox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-menu [menu-bar toolbox])

;; (make-submenu [menu-bar toolbox]
;;               :title "Toolbox")

;; (make-menu-item [menu-bar toolbox toolbox]
;;                 :title "Visit Toolbox"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (find-file "/m/Toolbox/Toolbox.adoc")))

;; (make-submenu [menu-bar toolbox config]
;;               :title "My config")

;; (make-menu-item [menu-bar toolbox config packages]
;;                 :title "Config packages"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (find-file "~/emacs-config/load-packages.el")))

;; (make-menu-item [menu-bar toolbox config personal]
;;                 :title "Config personal stuff"
;;                 :action (lambda ()
;;                           (interactive)
;;                           (find-file "~/emacs-config/load-personal-stuff.el")))

;; (make-submenu [menu-bar toolbox tags]
;;               :title "Tags")

;; (make-menu-item [menu-bar toolbox tags create-tags]
;;                 :title "Create tags"
;;                 :tooltip "Create a tag table from current directory."
;;                 :action '$create-tags)

;; (make-menu-item [menu-bar toolbox tags list-tags]
;;                 :title "List toolbox tags"
;;                 :action 'helm-etags-select)

;; (make-menu-item [menu-bar toolbox tags goto-tag]
;;                 :title "Go to tag"
;;                 :action 'goto-tag
;;                 :tooltip "Jump to declaration of a tag")

;; (make-menu-item [menu-bar toolbox tags goto-tag-other-window]
;;                 :title "Go to tag (other window)"
;;                 :action 'goto-tag-other-window
;;                 :tooltip "jump to declaration of a tag in another window")

;; (make-menu-separator [menu-bar toolbox separator])

;; (make-menu-item [menu-bar toolbox copy-dir]
;;                 :title "Copy directory"
;;                 :tooltip "Copy current directory to clipboard."
;;                 :action '~copy-directory)

;; (make-menu-item [menu-bar toolbox copy-file-path]
;;                 :title "Copy file path"
;;                 :tooltip "Copy current full file path to clipboard."
;;                 :action '~copy-file-path)

;; (make-menu-separator [menu-bar toolbox separator-2])

;; (make-menu-item [menu-bar toolbox xkcd]
;;                 :title "xkcd"
;;                 :action 'xkcd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literate programming - Ulqui integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ulqui:generate-html-current-dir ()
  "Generates HTML from and to current directory."
  (interactive)
  (message "Generating HTML...")
  (~exec-in-other-window "ulqui generate-html --from . --to generated-html/"))

(defun ulqui:generate-src-current-dir ()
  "Generates source code from and to current directory."
  (interactive)
  (message "Generating source...")
  (~exec-in-other-window "ulqui generate-src --from . --to generated-src/"))

(defun ~cl/next-snippet ()
  "Jumps to the next `eval'-able AsciiDoc snippet."
  (interactive)
  (cond ((re-search-forward "\\.\\(code\\|file\\).*\n\\[source,lisp" (point-max) t)
         (search-forward "----")
         (next-line)
         (beginning-of-line)
         (point))
        (t
         -1)))

(defun ~cl/compile-snippet ()
  "`eval's the current snippet with Common Lisp's Slime.  Note
that this function would not work reliably if the current point
is not inside a snippet."
  (interactive)
  (save-excursion
    (cond ((member major-mode '(lisp-mode common-lisp-mode))
           (beginning-of-buffer)
           (let ((start (point)))
             (end-of-buffer)
             (slime-compile-region start (point))))
          (t
           (re-search-backward "^----$")
           (next-line)
           (beginning-of-line)
           (let ((start (point)))
             (re-search-forward "^----$")
             (previous-line)
             (end-of-line)
             (slime-compile-region start (point)))))))

(defun ulqui:tmux-make ()
  "Runs `make' in current Tmux session."
  (interactive)
  (srun "make"))

(bind-key "<f5>"   '~cl/next-snippet adoc-mode-map)
(bind-key "<f6>"   '~cl/compile-snippet adoc-mode-map)
(bind-key "<S-f5>" 'ulqui:generate-src-current-dir)
(bind-key "<S-f6>" 'ulqui:generate-html-current-dir)
(bind-key "C-S-b"  'ulqui:tmux-make)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~cider-eval (&optional expr)
  "Eval an expression in Cider REPL."
  (interactive)
  (let ((expr (cond ((not (~string-empty? expr))
                     expr)
                    (t
                     (~read-string "Expression: "))))
        (cider-buffer (->> (buffer-list)
                        (-filter (lambda (buffer)
                                   (string-match-p "\\*cider-repl.*" (buffer-name buffer))))
                        first)))
    (unless (null cider-buffer)
      (set-buffer cider-buffer)
      (goto-char (point-max))
      (insert expr)
      (cider-repl-return)
      (~popup-buffer (buffer-name cider-buffer)))))

(defun ~cider-compile-file-and-run-main ()
  "Compile current file with Cider and run the `-main' function."
  (interactive)
  (call-interactively 'cider-load-current-buffer)
  (~cider-eval "(-main)"))

(defun* ~goto-sync-notes (arg)
  "Mimick `helm-find-files' to jump to my `~/Dropbox/cmpitg/Notes`
directory."
  (interactive "P")
  (let* ((hist          (and arg helm-ff-history (helm-find-files-history)))
         (default-input (expand-file-name "~/Dropbox/cmpitg/Notes/"))
         (input         (cond ((and (eq major-mode 'org-agenda-mode)
                                    org-directory
                                    (not default-input))
                               (expand-file-name org-directory))
                              ((and (eq major-mode 'dired-mode) default-input)
                               (file-name-directory default-input))
                              (default-input)
                              (t (expand-file-name (helm-current-directory)))))
         (presel        (helm-aif (or hist
                                      (buffer-file-name (current-buffer))
                                      (and (eq major-mode 'dired-mode)
                                           default-input))
                            (if helm-ff-transformer-show-only-basename
                                (helm-basename it) it))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input presel)))

(defalias 'srun '~send-keys-to-tmux)

(bind-key "C-c l m a" '~emacs-lisp-make-alias emacs-lisp-mode-map)
(bind-key "s-; v k k" '~visit-personal-keybindings)
(bind-key "s-; ; ;" '~insert-full-line-comment)
(bind-key "s-; b b" '~bind-key-temporary)
(bind-key "s-; d o" 'python-django-open-project)
(bind-key "M-:" '~add-bracket-and-eval)
(bind-key "<s-backspace>" 'srun)
(bind-key "s-; f f" '~goto-sync-notes)
(bind-key "<C-menu> <C-menu> C-e" 'emacs-lisp-mode)

(use-package paredit
  :config (progn
            (bind-key "C-c l (" '~parenthesize-last-sexp paredit-mode-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmpitg:visit-todo ()
  "Visit my TODO list at ~/Docs/Org/TODO.org"
  (interactive)
  (find-file "~/Docs/Org/TODO.org"))

(bind-key "s-; s-;" 'cmpitg:visit-todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some themes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package inertial-scroll :ensure t)
;; (require 'inertial-scroll)
;; (inertias-global-minor-mode 1)

;; (setq inertias-initial-velocity 50)
;; (setq inertias-friction 120)
;; (setq inertias-update-time 50)
;; (setq inertias-rest-coef 0.1)
;; (setq inertias-global-minor-mode-map
;;       (inertias-define-keymap
;;        '(
;;          ;; Mouse wheel scrolling
;;          ("<wheel-up>"   . inertias-down-wheel)
;;          ("<wheel-down>" . inertias-up-wheel)
;;          ("<mouse-4>"    . inertias-down-wheel)
;;          ("<mouse-5>"    . inertias-up-wheel)
;;          ;; Scroll keys
;;          ("<next>"  . inertias-up)
;;          ("<prior>" . inertias-down)
;;          ("C-v"     . inertias-up)
;;          ("M-v"     . inertias-down)
;;          ) inertias-prefix-key))

;;; ---

;; (use-package geiser-mode
;;   :config (progn
;;            (dolist (sym '(process-string if-let))
;;              (put sym 'scheme-indent-function 1)
;;              (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;            ;; (dolist (sym '(with-shell-commands))
;;            ;;   (put sym 'scheme-indent-function 0)
;;            ;;   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;            ;; (dolist (sym '(module
;;            ;;                module*))
;;            ;;   (put sym 'scheme-indent-function 2)
;;            ;;   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))
;;            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package acme-mouse
  :load-path "/m/src/acme-mouse")

(bind-key "<C-mouse-1>" '~exec|-select-output)
(global-unset-key (kbd "<C-down-mouse-1>"))

(bind-key "<S-mouse-1>" 'wand:execute)
(global-unset-key (kbd "<S-down-mouse-1>"))

(bind-key "<mouse-8>" 'evil-insert)
;; (bind-key "<mouse-8>" '(lambda ()
;;                          (interactive)
;;                          ;; (deactivate-mark t)
;;                          (call-interactively 'evil-insert)))
(bind-key "<mouse-9>" 'evil-force-normal-state)

(revert-all-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make shebang-ed files executable
(add-hook 'after-save-hook '~make-executable)

;; Clean up all Tramp remote connection before killing Emacs
(add-hook 'kill-emacs-hook '~clean-up-tramp)
