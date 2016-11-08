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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl)
(use-package cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some non-standard utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package misc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modern list processing library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashtable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ht
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package s
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/filesystem library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package f
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better swich-window.  'C-x o' now numbers windows and asks user to choice
;; which one to switch to interactively
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package switch-window
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode)

    (add-to-list 'projectile-globally-ignored-directories "node_modules*")
    (add-to-list 'projectile-globally-ignored-directories "*bower_components*")
    (add-to-list 'projectile-globally-ignored-directories "bower_components")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-files "*.pyc")
    (add-to-list 'projectile-globally-ignored-files ".*pyc")

    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-find-dir-includes-top-level t)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)

    ;; (setq projectile-switch-project-action 'projectile-dired)
    ;; (setq projectile-switch-project-action 'projectile-find-dir)

    (bind-key "M-v" nil)
    (bind-key "M-v b o" 'projectile-find-file-other-window projectile-mode-map)

    ;; Customize find file command via function
    ;; projectile-get-ext-command
    (setq projectile-git-command projectile-generic-command)
    (setq projectile-hg-command projectile-generic-command)))

(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :config
  (use-package helm
    :config (progn
              (helm-mode 1)
              (setq helm-boring-buffer-regexp-list '("\\*.+\\*"))

              ;; The following call to helm-follow-mode is local and has no
              ;; effect.  Read its documentation for the possible effect.
              ;; (helm-follow-mode 1)

              ;;
              ;; helm-follow currently comes with a cost that it opens every
              ;; file user is selecting in the Helm buffer. Add this when
              ;; you're absolutely sure you would open all files when visiting
              ;; them.
              ;;
              ;; (add-hook 'helm-after-update-hook #'(lambda ()
              ;;                                       (helm-follow-mode 1)))

              ;; Follow mode
              (eval-after-load "helm-multi-occur-1"
                '(progn
                   (helm-attrset 'follow 1 helm-source-moccur)))

              (eval-after-load "helm-occur-from-isearch"
                '(progn
                   (helm-attrset 'follow 1 helm-source-occur)))

              (eval-after-load "helm-occur"
                '(progn
                   (helm-attrset 'follow 1 helm-source-occur)))

              ;; Stupid bug makes me comment this line, why????
              (eval-after-load "helm-buffers-list"
                '(progn
                   (eval-after-load "helm-buffers"
                     '(progn
                        (helm-attrset 'follow
                                      1
                                      helm-source-buffers-list)))))

              (eval-after-load "helm-bookmark"
                '(progn
                   (helm-attrset 'follow 1 helm-source-bookmarks)))

              ;; Don't auto change-dir when find-file
              (setq-default helm-ff-auto-update-initial-value nil)

              ;; Fully enable fuzzy matching
              (setq helm-mode-fuzzy-match t
                    helm-completion-in-region-fuzzy-match t)

              ;;
              ;; Setting up Helm follow mode
              ;;
              ;; This is why I hate Helm developer:
              ;; https://github.com/emacs-helm/helm/issues/530
              ;;

              (helm-occur-init-source)

              (helm-attrset 'follow 1 helm-source-occur)
              (helm-attrset 'follow 1 helm-source-regexp)
              ;; (helm-attrset 'follow 1 helm-source-moccur)

              (bind-key "<backtab>" 'helm-execute-persistent-action helm-map)
              (bind-key "<f12>" 'helm-minibuffer-history minibuffer-local-map)
              (bind-key "<S-f1>" 'helm-minibuffer-history minibuffer-local-map)

              (when (executable-find "curl")
                (setq helm-google-suggest-use-curl-p t))

              ;; Open Helm buffer inside current window, do not disturb other
              ;; window
              (setq helm-split-window-in-side-p t)

              ;; Make Helm items cycleable
              ;; (setq helm-move-to-line-cycle-in-source t)

              ;; Search for library in `require' and `declare-function'
              ;; (setq helm-ff-search-library-in-sexp t)

              ;; Some fuzzy matching option
              (setq helm-M-x-fuzzy-match t)
              (setq helm-buffers-fuzzy-matching t)
              (setq helm-recentf-fuzzy-match t)
              (setq helm-semantic-fuzzy-match t)
              (setq helm-imenu-fuzzy-match t)
              (setq helm-locate-fuzzy-match t)
              (setq helm-apropos-fuzzy-match t)
              (setq helm-lisp-fuzzy-completion t)

              (setq helm-ff-file-name-history-use-recentf t))))

;; Project management with Helm Projectile
(use-package helm-projectile
  :ensure t
  :config
  (progn
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)
    (setq projectile-require-project-root t)
    (setq projectile-switch-project-action 'projectile-dired)

    ;; A bug in projectile ignore that doesn't ignore
    (setq projectile-indexing-method 'native)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History utility, remebering and jumping to places
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package history
  :ensure t
  :diminish history-mode
  :config (progn
            (setq history-history-max 256
                  history-window-local-history nil)
            (history-mode 1)

            (bind-key "s-h" 'history-preview-prev-history history-map)
            (bind-key "s-n" 'history-preview-next-history history-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file processing with Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various actions with 'thing' at current cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package thingatpt
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure t)

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
  :ensure t
  :diminish eldoc-mode
  :config (progn
            (eldoc-add-command 'paredit-backward-delete
                               'paredit-close-round)

            (add-hook 'emacs-lisp-mode-hook        'turn-on-eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook  'turn-on-eldoc-mode)
            (add-hook 'ielm-mode-hook              'turn-on-eldoc-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jonathan Chu's version of powerline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install from local
;;
;; https://github.com/jonathanchu/emacs-powerline
;;

(load-file (~get-config "local-packages/powerline/powerline.el"))
(use-package powerline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Just to hide undo-tree mode from mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :diminish undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better popup window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/m2ym/popwin-el

(use-package popwin
  :ensure t
  :config (progn
            (popwin-mode 1)
            (setq anything-samewindow nil)

            (dolist (el '(("\*anything*" :regexp t :height 25)
                          ("*anything*" :height 20)
                          (dired-mode :position top)
                          "*Backtrace*"
                          "*Shell Command Output*"
                          (compilation-mode :noselect t)
                          "*slime-apropos*"
                          "*slime-macroexpansion*"
                          "*slime-description*"
                          ("*slime-compilation*" :noselect t)
                          "*slime-xref*"
                          (sldb-mode :stick t)
                          slime-repl-mode
                          slime-connection-list-mode
                          "*vc-diff*"
                          "*vc-change-log*"
                          (" *undo-tree*" :width 0.3 :position right)))
              (push el popwin:special-display-config))))

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
;; Support for tar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tar-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save and restore current editing point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package saveplace
  :ensure t
  :init (progn
          (setq-default save-place t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package color-theme
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :ensure t
  :init (progn
          (recentf-mode 1)
          (setq recentf-max-menu-items 256)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smoother scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smooth-scrolling
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm mode seems to be much convenient than ido
;;

;; (use-package ido
;;   :ensure ido
;;   :config (progn
;;             (ido-mode 1)
;;             (ido-everywhere 1)
;;             (use-package flx-ido
;;               :config (progn
;;                         (flx-ido-mode 1)

;;                         ;; disable ido faces to see flx highlights.
;;                         (setq ido-use-faces nil)))
;;             (use-package ido-vertical-mode
;;               :ensure ido-vertical-mode
;;               :config (ido-vertical-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom unique naming method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package uniquify
  :init (progn
          (setq uniquify-buffer-name-style 'forward)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jump between occurrences of a symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `smartscan-symbol-go-forward'   M-n
;; `smartscan-symbol-go-backward'  M-p

(use-package smartscan
  :ensure t
  :config (progn
            (global-smartscan-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open with external program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load from local
;;
;; Seems to be abandoned.  We'll find another way to provide the openwith
;; functionality.
;;

;; (use-package openwith
;;   :config (progn
;;             (openwith-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better M-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helm-M-x should be better than smex
;;

;; (use-package smex
;;   :ensure t
;;   :init (progn
;;           (smex-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asciidoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package adoc-mode
  :ensure t
  :defer t
  :commands adoc-mode
  :init (progn
          (~auto-load-mode '("\\.ascii" "\\.adoc") 'adoc-mode)
          (add-hook 'adoc-mode-hook 'auto-fill-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find file with fuzzy matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm is mature and advanced and serves the needs well.  Perhaps I'll come
;; back to fiplr some time.
;;

;; (use-package fiplr
;;   :ensure fiplr
;;   :config (progn
;;             (add-to-list 'fiplr-root-markers "README.md")
;;             (add-to-list 'fiplr-root-markers "README.adoc")
;;             (add-to-list 'fiplr-root-markers "README.txt")
;;             (add-to-list 'fiplr-root-markers "README")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editable Ack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep-ack
  :ensure wgrep-ack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsable kill ring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package browse-kill-ring
  :ensure browse-kill-ring
  :config (progn
            (browse-kill-ring-default-keybindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick jumping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :commands ace-jump-mode
  :init (progn
          (bind-key "s-k" 'ace-jump-mode)
          (bind-key "C-x SPC" 'ace-jump-mode-pop-mark))
  :config (progn
            (ace-jump-mode-enable-mark-sync)
            ;; * Without prefix, ace-jump directs toward char
            ;; * With 1 prefix, line
            ;; * With 2 prefixes, word
            (setq ace-jump-mode-submode-list
                  '(ace-jump-char-mode
                    ace-jump-line-mode
                    ace-jump-word-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer navigation with pattern matching and replacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Seems unstable, needs more evaluation
;;

(use-package swoop
  :ensure t
  :commands swoop
  :init (use-package helm-swoop
          :ensure helm-swoop))

;; (defun ~swoop-bare (&optional $query)
;;   "Search through words within the current buffer.  Don't take
;; current word at point as initial string."
;;   (interactive)
;;   (if current-prefix-arg
;;       (swoop-core :$resume t :$query swoop-last-query-plain)
;;     (swoop-core :$query "")))

;; (defun* ~helm-swoop-bare (&key $query $source ($multiline current-prefix-arg))
;;   "Search through words within the current buffer.  Don't take
;; current word at point as initial string."
;;   (interactive)
;;   (setq helm-swoop-synchronizing-window (selected-window))
;;   (setq helm-swoop-last-point (cons (point) (buffer-name (current-buffer))))
;;   (setq helm-swoop-last-line-info
;;         (cons (current-buffer) (line-number-at-pos)))
;;   (unless (boundp 'helm-swoop-last-query)
;;     (set (make-local-variable 'helm-swoop-last-query) ""))
;;   (setq helm-swoop-target-buffer (current-buffer))
;;   (helm-swoop--set-prefix (prefix-numeric-value $multiline))
;;   ;; Overlay
;;   (setq helm-swoop-line-overlay (make-overlay (point) (point)))
;;   (overlay-put helm-swoop-line-overlay
;;                'face (if (< 1 helm-swoop-last-prefix-number)
;;                          'helm-swoop-target-line-block-face
;;                        'helm-swoop-target-line-face))
;;   ;; Cache
;;   (cond ((not (boundp 'helm-swoop-cache))
;;          (set (make-local-variable 'helm-swoop-cache) nil))
;;         ((buffer-modified-p)
;;          (setq helm-swoop-cache nil)))
;;   ;; Cache for multiline
;;   (cond ((not (boundp 'helm-swoop-list-cache))
;;          (set (make-local-variable 'helm-swoop-list-cache) nil))
;;         ((buffer-modified-p)
;;          (setq helm-swoop-list-cache nil)))
;;   (unwind-protect
;;       (progn
;;         ;; For synchronizing line position
;;         (ad-enable-advice 'helm-next-line 'around 'helm-swoop-next-line)
;;         (ad-activate 'helm-next-line)
;;         (ad-enable-advice 'helm-previous-line 'around 'helm-swoop-previous-line)
;;         (ad-activate 'helm-previous-line)
;;         (ad-enable-advice 'helm-move--next-line-fn 'around
;;                           'helm-multi-swoop-next-line-cycle)
;;         (ad-activate 'helm-move--next-line-fn)
;;         (ad-enable-advice 'helm-move--previous-line-fn 'around
;;                           'helm-multi-swoop-previous-line-cycle)
;;         (ad-activate 'helm-move--previous-line-fn)
;;         (add-hook 'helm-update-hook 'helm-swoop--pattern-match)
;;         (add-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position t)
;;         (unless (and (symbolp 'helm-match-plugin-mode)
;;                      (symbol-value 'helm-match-plugin-mode))
;;           (helm-match-plugin-mode 1))
;;         (cond ($query
;;                (if (string-match
;;                     "\\(\\^\\[0\\-9\\]\\+\\.\\)\\(.*\\)" $query)
;;                    $query ;; NEED FIX #1 to appear as a "^"
;;                  $query))
;;               (mark-active
;;                (let (($st (buffer-substring-no-properties
;;                            (region-beginning) (region-end))))
;;                  (if (string-match "\n" $st)
;;                      (message "Multi line region is not allowed")
;;                    (setq $query (helm-swoop-pre-input-optimize $st)))))
;;               (t (setq $query "")))
;;         ;; First behavior
;;         (helm-swoop--recenter)
;;         (move-beginning-of-line 1)
;;         (helm-swoop--target-line-overlay-move)
;;         ;; Execute helm
;;         (let ((helm-display-function helm-swoop-split-window-function)
;;               (helm-display-source-at-screen-top nil)
;;               (helm-completion-window-scroll-margin 5))
;;           (helm :sources
;;                 (or $source
;;                     (if (> helm-swoop-last-prefix-number 1)
;;                         (helm-c-source-swoop-multiline helm-swoop-last-prefix-number)
;;                       (helm-c-source-swoop)))
;;                 :buffer helm-swoop-buffer
;;                 :input $query
;;                 :prompt helm-swoop-prompt
;;                 :preselect
;;                 ;; Get current line has content or else near one
;;                 (if (string-match "^[\t\n\s]*$" (helm-swoop--get-string-at-line))
;;                     (save-excursion
;;                       (if (re-search-forward "[^\t\n\s]" nil t)
;;                           (format "^%s\s" (line-number-at-pos))
;;                         (re-search-backward "[^\t\n\s]" nil t)
;;                         (format "^%s\s" (line-number-at-pos))))
;;                   (format "^%s\s" (line-number-at-pos)))
;;                 :candidate-number-limit helm-swoop-candidate-number-limit)))
;;     ;; Restore helm's hook and window function etc
;;     (helm-swoop--restore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL shortener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package url-shortener
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple tabbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package tabbar-ruler
;;   :ensure tabbar-ruler
;;   :config (progn
;;             (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;             ;; (setq tabbar-ruler-global-ruler t)     ; if you want a global ruler
;;             ;; (setq tabbar-ruler-popup-menu t)       ; If you want a popup menu.
;;             ;; (setq tabbar-ruler-popup-toolbar t)    ; If you want a popup toolbar
;;             ;; (setq tabbar-ruler-popup-scrollbar t)) ; If you want to only show
;;             ;;                                        ; the scroll bar when your
;;             ;;                                        ; mouse is moving.
;;             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better grep'ing with ag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AckFull, wgrep, ack-and-a-half are all abandonware.
;;

;;
;; Use interactively
;;

(load-file (~get-config "local-packages/ack-and-a-half/ack-and-a-half.el"))
(use-package ack-and-a-half
  :init (progn
          ;; Fix Debian-based distros' executable file
          (setq ack-and-a-half-executable (or (executable-find "ack-grep")
                                              (executable-find "ack")))
          ;; (setq ack-and-a-half-prompt-for-directory 'unless-guessed)
          (setq ack-and-a-half-prompt-for-directory t)
          (defalias 'ack 'ack-and-a-half)
          (defalias 'ack-same 'ack-and-a-half-same)
          (defalias 'ack-find-file 'ack-and-a-half-find-file)
          (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; (use-package ag
;;   :ensure t)

;; (use-package wgrep-ag
;;   :ensure t)

;;
;; Use for one-off searches (as the Helm buffer is not persistent
;;
(use-package helm-ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m web browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package w3m
  :ensure t
  :commands w3m-browse-url
  :init (progn
          (setq browse-url-browser-function 'w3m-browse-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package menu-bar+
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load before auto complete

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (progn
            (add-to-list 'yas-snippet-dirs (expand-file-name *snippet-dir*))
            (yas-global-mode 1)))

(use-package dired-details+
  :init (progn
          (use-package dired-single))
  :config (progn
            (setq dired-listing-switches "-lhFgG --group-directories-first")))

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
                                  company-active-map)))

            (use-package helm-company
              :ensure t
              :config (progn
                        (bind-key "M-RET" 'helm-company company-mode-map)
                        (bind-key "M-RET" 'helm-company company-active-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting phrase and expression when needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hi-lock
  :ensure t
  :commands (highlight-phrase highlight-regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gist integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gist
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (progn
            ;; (use-package markdown-mode+
            ;;   :ensure markdown-mode+)

            (add-hook 'markdown-mode-hook 'auto-fill-mode)
            (custom-set-faces
             ;; Your init file should contain only one such instance.
             ;; If there is more than one, they won't work right.
             '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7 :background "#ABCDEF"))))
             '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5 :background "green"))))
             '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-pairing brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens-config
  :ensure smartparens
  :config (progn
            (eval-after-load 'smartparens
              '(progn
                 ;; (defadvice smartparens-mode (around disable-autopairs-around (arg))
                 ;;   "Disable autopairs mode if smartparens-mode is turned on."
                 ;;   ad-do-it
                 ;;   (autopair-mode 0))

                 ;; (ad-activate 'smartparens-mode)

                 (smartparens-global-mode)))))

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
            (add-hook 'scheme-mode-hook           '~load-paredit-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Async eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'later-do)
(use-package later-do
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple scratch buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'multi-scratch)
(use-package multi-scratch
  :commands multi-scratch-new)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulating Firefox/Thunderbird
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'moz-repl)
(use-package moz
  :config (progn
            (add-hook 'javascript-mode-hook '-setup-moz-javascript)
            (add-hook 'js3-mode-hook        '-setup-moz-javascript)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display trailing whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'whitespace)
(use-package whitespace
  :commands whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; En/decoding JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'json-mode)

(use-package json-mode
  :defer t
  :mode "\\.json\\'"
  :init (progn
          (eval-after-load "json-mode"
            '(progn
              (setq c-basic-offset 2)
              (setq tab-width 2)))))

(use-package json
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Github Gist management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gist
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Side-bar directory tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; https://github.com/sabof/project-explorer
;;
;; (use-package project-explorer
;;   :ensure t)

;;
;; Nav is tested, good enough to go
;;

;; https://github.com/ancane/emacs-nav

;; (use-package nav
;;   :load-path "/m/src/emacs-nav"
;;   :config (progn
;;             (nav-disable-overeager-window-splitting)))
;;           (progn

;; (eval-after-load "evil-mode"
;;   '(progn
;;      (bind-key "SPC o a" 'nav-toggle evil-normal-state-map)))

;;
;; Neotree might break tab completion in minibuffer
;;
;; https://github.com/jaypei/emacs-neotree
;; https://www.emacswiki.org/emacs/NeoTree
;;

(use-package neotree
  :ensure t
  :config
  (progn
    ;; (setq neo-theme 'icons)
    (setq neo-theme 'arrow)

    (setq projectile-switch-project-action
          'neotree-projectile-action)

    (add-hook 'neotree-mode-hook
              (lambda ()
                (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
                (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
                (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
                (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)))

    ;; If you use popwin, when NeoTree is open and successively a temporary
    ;; buffer is opened with popwin, a new window with the NeoTree buffer is
    ;; displayed side by side next to the first one (#50). This code will help
    ;; you

    ;; (when neo-persist-show
    ;;   (add-hook 'popwin:before-popup-hook
    ;;             (lambda () (setq neo-persist-show nil)))
    ;;   (add-hook 'popwin:after-popup-hook
    ;;             (lambda () (setq neo-persist-show t))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ee:load-essential-packages)
