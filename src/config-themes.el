;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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
;; Color theme
;;
;; Ref: https://www.emacswiki.org/emacs/ColorThemes
;;

(use-package color-theme)

;;
;; Load all themes
;;

(apply #'~load-files (directory-files (~get-config "themes/")))

;;
;; Theming
;;

;; Use this if you don't use any theme
;; (set-face-foreground 'font-lock-comment-face "#3a345f")
;; (set-background-color "#f6f6f6")
;; (set-background-color "#efefef")

;; Current
;; (require 'color-theme-acme-light)
;; (color-theme-acme-light)
(load-theme 'plan9 t)
;; (color-theme-textmate-modified)
;; (load-theme 'afternoon t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring themes")

(provide 'rmacs:config-themes)
