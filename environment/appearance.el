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
