;;
;; Copyright (C) 2012-2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
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
;; Load all themes in ./themes/
;;

(apply #'~load-files
       (~list-dir-full-path (~get-local-config-dir "themes/")))

;;
;; Theming and stuff
;;

;; Using system font

(setq font-use-system-font t)

;; (set-face-foreground 'font-lock-comment-face "#3a345f")
(set-face-attribute 'font-lock-comment-face nil :foreground "#3a345f")

;; (set-cursor-color "cyan")
;; (set-cursor-color "gray")
(set-cursor-color "black")

;; (set-background-color "#f2f2f2")
(set-background-color "#efefef")

;;
;; Themes
;;

;; (color-theme-textmate-modified)
;; (color-theme-molokai)
;; (color-theme-zenburn)
;; (color-theme-textmate)
;; (color-theme-twilight)
;; (inspiration-144382)
;; (inspiration-648409)
;; (inspiration-990434)
;; (color-theme-charcoal-black)
;; (color-theme-calm-forest)

;;
;; Misc
;;

;; Hide the toolbar
(tool-bar-mode -1)

;; Hide the scrollbar
(scroll-bar-mode -1)

;; Hide the menu bar
;; (menu-bar-mode -1)
(menu-bar-mode 1)

;; Display time
(display-time)

;; Turn off welcome message
(setq inhibit-startup-message t)

;; Display the size of the buffer
(size-indication-mode 1)

;; Numbering lines
(use-package linum
  :init (progn
          (global-linum-mode 1)
          (setq linum-format "%d ")))

;; Show the battery indicator
(display-battery-mode 1)

;;;; Use font lock
(global-font-lock-mode t)
(setq font-lock-maximum-size nil)

;; Set transparency
;; (set-frame-parameter nil 'alpha 78)

;;; Blink cursor
(blink-cursor-mode t)

;;; Set frame title
(setq frame-title-format
      '(multiple-frames "%b" ("@" system-name )))

;; read-quoted-char-radix (10 or 16)

;; Set the appropriate frame size

;; (set-frame-size-according-to-resolution)

;; Custom variables

(custom-set-variables
 ;; '(column-number-mode t)
 ;; '(display-battery-mode t)
 ;; '(display-time-mode t)
 ;; '(size-indication-mode t)
 '(face-font-family-alternatives (quote (("Monaco" "Consolas" "Monospace")
                                         ("Monaco" "Consolas" "CMU Typewriter Text" "fixed")
                                         ("Geneva" "Sans Serif" "helv" "helvetica" "arial" "fixed")
                                         ("helv" "helvetica" "arial" "fixed"))))
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10) (encoding . utf-8))))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; Custom fonts

(custom-set-faces
 '(default ((t (:inherit nil
                         :stipple nil
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         ;; :height 98
                         :width normal
                         :foundry "unknown"
                         :family "Monaco"))))
 ;; '(mode-line ((t (:background "grey75"
 ;;                              :foreground "#3d3d3d"
 ;;                              :inverse-video t
 ;;                              :box (:line-width 1 :color "#000000" :style released-button)
 ;;                              :slant normal
 ;;                              :weight normal
 ;;                              :height 100
 ;;                              :family "Geneva"))))
 ;; '(mode-line ((t (:inverse-video t
 ;;                                 :slant normal
 ;;                                 :weight normal
 ;;                                 :height 100
 ;;                                 :family "Geneva"))))

 '(rst-level-1-face ((t (:embolden t))) t))

;; Change cursor type

;;(set-default 'cursor-type 'hbar)
(set-default 'cursor-type 'bar)
;;(set-default 'cursor-type 'box)

;; (show-paren-mode 1)
;; (show-paren-mode -1)
;; (setq show-paren-delay 0)
;; (setq show-paren-style 'expression)
