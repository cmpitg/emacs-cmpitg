;;
;; Copyright (C) 2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
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
  :ensure dash
  :config (dash-enable-font-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hashtable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ht
  :ensure ht)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package s
  :ensure s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File/filesystem library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package f
  :ensure f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-config
  :ensure helm
  :commands (helm-find-files helm-buffers-list helm-bookmarks)
  :config (use-package helm
            :config (progn
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
                                (helm-attrset 'follow 1 helm-source-buffers-list)))))

                      ;; (eval-after-load "helm-bookmark"
                      ;;   '(progn
                      ;;      (helm-attrset 'follow 1 helm-source-bookmarks)))

                      ;; (helm-attrset 'follow 0 helm-source-find-files)

                      ;; Don't auto change-dir when find-file
                      (setq-default helm-ff-auto-update-initial-value nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file processing with Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various actions with 'thing' at current cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package thingatpt
  :ensure thingatpt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :ensure multiple-cursors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :ensure expand-region
  :commands er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live function signature at echo area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eldoc
  :ensure eldoc
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
;; Better popup window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/m2ym/popwin-el

(use-package popwin
  :ensure popwin
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
  :ensure dired+
  :init (progn
          (setq dired-listing-switches "-lahF")
          ;; Reuse current buffer when opening file/dir
          (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired-details+
  :ensure dired-details+
  :init (progn
          (use-package dired-single
            :ensure dired-single))
  :config (progn
            (setq dired-listing-switches "-lhFgG --group-directories-first")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for tar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tar-mode
  :ensure tar-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save and restore current editing point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package saveplace
  :ensure saveplace
  :init (progn
          (setq-default save-place t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package color-theme
  :ensure color-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :ensure recentf
  :commands recentf-open-files
  :idle (recentf-mode)
  :idle-priority 3
  :init (progn
          (recentf-mode 1)
          (setq recentf-max-menu-items 128)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smoother scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smooth-scrolling
  :ensure smooth-scrolling)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ido
  :ensure ido
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (use-package flx-ido
              :config (progn
                        (flx-ido-mode 1)

                        ;; disable ido faces to see flx highlights.
                        (setq ido-use-faces nil)))
            (use-package ido-vertical-mode
              :ensure ido-vertical-mode
              :config (ido-vertical-mode 1))))

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
  :ensure smartscan
  :config (progn
            (global-smartscan-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open with external program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load from local
;;

(use-package openwith
  :init (progn
          (openwith-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better M-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smex
  :ensure smex
  :init (progn
          (smex-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asciidoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package adoc-mode
  :ensure adoc-mode
  :defer t
  :commands adoc-mode
  :init (progn
          (~auto-load-mode '("\\.ascii" "\\.txt" "\\.adoc") 'adoc-mode)
          (add-hook 'adoc-mode-hook 'auto-fill-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find file with fuzzy matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package fiplr
  :ensure fiplr
  :config (progn
            (add-to-list 'fiplr-root-markers "README.md")
            (add-to-list 'fiplr-root-markers "README.adoc")
            (add-to-list 'fiplr-root-markers "README.txt")
            (add-to-list 'fiplr-root-markers "README")))

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
            (ace-jump-mode-enable-mark-sync)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer navigation with pattern matching and replacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package swoop
  :ensure swoop
  :commands swoop
  :init (use-package helm-swoop
          :ensure helm-swoop))

(defun ~swoop-bare (&optional $query)
  "Search through words within the current buffer.  Don't take
current word at point as initial string."
  (interactive)
  (if current-prefix-arg
      (swoop-core :$resume t :$query swoop-last-query-plain)
    (swoop-core :$query "")))

(defun* ~helm-swoop-bare (&key $query $source ($multiline current-prefix-arg))
  "Search through words within the current buffer.  Don't take
current word at point as initial string."
  (interactive)
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point (cons (point) (buffer-name (current-buffer))))
  (setq helm-swoop-last-line-info
        (cons (current-buffer) (line-number-at-pos)))
  (unless (boundp 'helm-swoop-last-query)
    (set (make-local-variable 'helm-swoop-last-query) ""))
  (setq helm-swoop-target-buffer (current-buffer))
  (helm-swoop--set-prefix (prefix-numeric-value $multiline))
  ;; Overlay
  (setq helm-swoop-line-overlay (make-overlay (point) (point)))
  (overlay-put helm-swoop-line-overlay
               'face (if (< 1 helm-swoop-last-prefix-number)
                         'helm-swoop-target-line-block-face
                       'helm-swoop-target-line-face))
  ;; Cache
  (cond ((not (boundp 'helm-swoop-cache))
         (set (make-local-variable 'helm-swoop-cache) nil))
        ((buffer-modified-p)
         (setq helm-swoop-cache nil)))
  ;; Cache for multiline
  (cond ((not (boundp 'helm-swoop-list-cache))
         (set (make-local-variable 'helm-swoop-list-cache) nil))
        ((buffer-modified-p)
         (setq helm-swoop-list-cache nil)))
  (unwind-protect
      (progn
        ;; For synchronizing line position
        (ad-enable-advice 'helm-next-line 'around 'helm-swoop-next-line)
        (ad-activate 'helm-next-line)
        (ad-enable-advice 'helm-previous-line 'around 'helm-swoop-previous-line)
        (ad-activate 'helm-previous-line)
        (ad-enable-advice 'helm-move--next-line-fn 'around
                          'helm-multi-swoop-next-line-cycle)
        (ad-activate 'helm-move--next-line-fn)
        (ad-enable-advice 'helm-move--previous-line-fn 'around
                          'helm-multi-swoop-previous-line-cycle)
        (ad-activate 'helm-move--previous-line-fn)
        (add-hook 'helm-update-hook 'helm-swoop--pattern-match)
        (add-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position t)
        (unless (and (symbolp 'helm-match-plugin-mode)
                     (symbol-value 'helm-match-plugin-mode))
          (helm-match-plugin-mode 1))
        (cond ($query
               (if (string-match
                    "\\(\\^\\[0\\-9\\]\\+\\.\\)\\(.*\\)" $query)
                   $query ;; NEED FIX #1 to appear as a "^"
                 $query))
              (mark-active
               (let (($st (buffer-substring-no-properties
                           (region-beginning) (region-end))))
                 (if (string-match "\n" $st)
                     (message "Multi line region is not allowed")
                   (setq $query (helm-swoop-pre-input-optimize $st)))))
              (t (setq $query "")))
        ;; First behavior
        (helm-swoop--recenter)
        (move-beginning-of-line 1)
        (helm-swoop--target-line-overlay-move)
        ;; Execute helm
        (let ((helm-display-function helm-swoop-split-window-function)
              (helm-display-source-at-screen-top nil)
              (helm-completion-window-scroll-margin 5))
          (helm :sources
                (or $source
                    (if (> helm-swoop-last-prefix-number 1)
                        (helm-c-source-swoop-multiline helm-swoop-last-prefix-number)
                      (helm-c-source-swoop)))
                :buffer helm-swoop-buffer
                :input $query
                :prompt helm-swoop-prompt
                :preselect
                ;; Get current line has content or else near one
                (if (string-match "^[\t\n\s]*$" (helm-swoop--get-string-at-line))
                    (save-excursion
                      (if (re-search-forward "[^\t\n\s]" nil t)
                          (format "^%s\s" (line-number-at-pos))
                        (re-search-backward "[^\t\n\s]" nil t)
                        (format "^%s\s" (line-number-at-pos))))
                  (format "^%s\s" (line-number-at-pos)))
                :candidate-number-limit helm-swoop-candidate-number-limit)))
    ;; Restore helm's hook and window function etc
    (helm-swoop--restore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL shortener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package url-shortener
  :ensure url-shortener
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
;; Better ack interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file (~get-config "local-packages/ack-and-a-half/ack-and-a-half.el"))
(use-package ack-and-a-half
  :init (progn
          ;; Fix Debian-based distros' executable file
          (setq ack-and-a-half-executable (or (executable-find "ack-grep")
                                              (executable-find "ack")))
          (defalias 'ack 'ack-and-a-half)
          (defalias 'ack-same 'ack-and-a-half-same)
          (defalias 'ack-find-file 'ack-and-a-half-find-file)
          (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m web browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package w3m
  :ensure w3m
  :commands w3m-browse-url
  :init (progn
          (setq browse-url-browser-function 'w3m-browse-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package menu-bar+
  :ensure menu-bar+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load before auto complete

(use-package yasnippet
  :ensure yasnippet
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

(use-package auto-complete
  :diminish auto-complete-mode
  :ensure auto-complete
  :init (progn
          (require 'auto-complete-config)
          (ac-config-default)
          (setq ac-sources
                '(ac-source-filename
                  ac-source-functions
                  ;; ac-source-yasnippet
                  ac-source-variables
                  ac-source-symbols
                  ac-source-features
                  ac-source-abbrev
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))

          (auto-complete-mode 1)
          (setq ac-fuzzy-enable t)

          (add-hook 'ruby-mode-hook
                    (lambda ()
                      (add-to-list 'ac-sources 'ac-source-rsense-method)
                      (add-to-list 'ac-sources 'ac-source-rsense-constant)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting phrase and expression when needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hi-lock
  :ensure hi-lock
  :commands (highlight-phrase highlight-regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gist integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gist
  :ensure gist
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure yaml-mode
  :commands yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure markdown-mode
  :commands markdown-mode
  :init (progn
          (~auto-load-mode '("\\.md$" "\\.markdown$") 'markdown-mode))
  :config (progn
            (use-package markdown-mode+
              :ensure markdown-mode+)

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
  :ensure paredit
  :config (progn
            (defadvice paredit-mode (around disable-otherparenslib-around (arg))
              "Disable autopairs mode if paredit-mode is turned on."
              ad-do-it
              (cond ((null ad-return-value)
                     (smartparens-mode 1))
                    (t
                     (smartparens-mode 0))))

            (ad-activate 'paredit-mode)

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
  :ensure json
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ee:load-essential-packages)
