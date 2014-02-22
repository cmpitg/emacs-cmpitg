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

;;
;; Install all essential packages
;;

(dolist (package *essential-packages*)
  ;; They're all available with Elpa or Melpa
  (unless (package-installed-p package)
    (package-install package)))

;; openwith is an exception, it's installed with el-get

(unless (el-get-package-is-installed 'openwith)
  (el-get-install 'openwith))

;;
;; Now, load them
;;

(use-package cl)                        ; Common Lisp subset
(use-package cl-lib)                    ; Some Common Lisp libraries
(use-package s)                         ; Better string APIs
(use-package f)                         ; Better APIs to work with file and
                                        ; directories
(use-package ht)                        ; Better hashtable APIs


;; Dash

;; Docs:
;;   https://github.com/magnars/dash.el

(use-package dash
  :init (progn
          (dash-enable-font-lock)))

;; Helm

(use-package helm
  :init (progn
          (require 'helm-config)))

(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

;; Don't auto change-dir
(setq-default helm-ff-auto-update-initial-value nil)

;; Smex for enhancing M-x

(use-package smex
  :init (progn
          (smex-initialize)))

;; Smooth scrolling
;;   http://www.emacswiki.org/emacs/SmoothScrolling

(use-package smooth-scrolling)

;; Popwin

;; https://github.com/m2ym/popwin-el
;;
;; | Key    | Command                               |
;; |--------+---------------------------------------|
;; | b      | popwin:popup-buffer                   |
;; | l      | popwin:popup-last-buffer              |
;; | o      | popwin:display-buffer                 |
;; | C-b    | popwin:switch-to-last-buffer          |
;; | C-p    | popwin:original-pop-to-last-buffer    |
;; | C-o    | popwin:original-display-last-buffer   |
;; | SPC    | popwin:select-popup-window            |
;; | s      | popwin:stick-popup-window             |
;; | 0      | popwin:close-popup-window             |
;; | f, C-f | popwin:find-file                      |
;; | e      | popwin:messages                       |
;; | C-u    | popwin:universal-display              |
;; | 1      | popwin:one-window                     |

(use-package popwin
  :init (progn
          (popwin-mode 1)

          (push '("\*anything*" :regexp t :height 20) popwin:special-display-config)

          (setq anything-samewindow nil)

          (push '("*anything*" :height 20)            popwin:special-display-config)

          (push '(dired-mode :position top)           popwin:special-display-config)

          (push "*Backtrace*"                         popwin:special-display-config)
          (push "*Shell Command Output*"              popwin:special-display-config)
          (push '(compilation-mode :noselect t)       popwin:special-display-config)

          ;; slime
          (push "*slime-apropos*"                     popwin:special-display-config)
          (push "*slime-macroexpansion*"              popwin:special-display-config)
          (push "*slime-description*"                 popwin:special-display-config)
          (push '("*slime-compilation*" :noselect t)  popwin:special-display-config)
          (push "*slime-xref*"                        popwin:special-display-config)
          (push '(sldb-mode :stick t)                 popwin:special-display-config)
          (push 'slime-repl-mode                      popwin:special-display-config)
          (push 'slime-connection-list-mode           popwin:special-display-config)

          ;; vc
          (push "*vc-diff*"                           popwin:special-display-config)
          (push "*vc-change-log*"                     popwin:special-display-config)

          ;; undo-tree
          (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)))

;; Smartscan

;; `smartscan-symbol-go-forward' - M-n
;; `smartscan-symbol-go-backward' - M-p

(use-package smartscan
  :init (progn
          (global-smartscan-mode 1)))

(use-package misc)

;; Better ido for minibuffer completion

(use-package ido
  :config (progn
            (use-package flx-ido
              :init (progn
                      (ido-mode 1)
                      (ido-everywhere 1)
                      (flx-ido-mode 1)

                      ;; disable ido faces to see flx highlights.
                      (setq ido-use-faces nil)))))

;; Open with external programs

(use-package openwith
  :init (progn
          (openwith-mode t)))

;; Thing at point

(use-package thingatpt)

;; Multiple cursors

(use-package multiple-cursors)

;; Expand region

(use-package expand-region)

;; Find file with fuzzy matching

(use-package fiplr
  :config (progn
            (add-to-list 'fiplr-root-markers "README.md")))

;; Themes

(use-package color-theme)

;; Save cursor position each time you open a file
;;   http://ergoemacs.org/emacs/emacs_save_cursor_position.html

(use-package saveplace
  :init (progn
          (setq save-place-file "~/emacs-save-places")
          (setq-default save-place t)))

;; Recent files

(use-package recentf
  :init (progn
          ;; Enable recent files
          (recentf-mode 1)))

;; Desktop mode, keeping workspace

(use-package desktop)

;; Working with tarballs

(use-package tar-mode)

;;; Custom unique naming method

(use-package uniquify
  :init (progn
          (setq uniquify-buffer-name-style 'forward)))

;;; Custom dired

(use-package dired+
  :init (progn
          (setq dired-listing-switches "-lahF")))

;; Icicles for better minibuffer completion
;;
;; http://www.emacswiki.org/emacs/Icicles

(use-package icicles
  :diminish icicle-mode
  :config (progn
            (icy-mode 1)))

;; Undo tree

(use-package undo-tree
  :config (progn
            (global-undo-tree-mode 1)))

;; Acme mouse

(use-package acme-mouse)

;; Edittable ack

(use-package wgrep-ack)

;; GPG interface

(use-package epa-file
  :config (progn
            (epa-file-enable)))
