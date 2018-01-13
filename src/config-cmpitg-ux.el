;;  -*- lexical-binding: t; -*-

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
;; Load all themes
;;

(dolist (theme-file (directory-files (~get-config "themes/")))
  (when (s-ends-with? ".el" theme-file)
    (load (~get-config "themes/" theme-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using system font

(setq font-use-system-font t)

;; (set-face-foreground 'font-lock-comment-face "#3a345f")
;;; Use this if you don't use any theme
;; (set-face-attribute 'font-lock-comment-face nil :foreground "#3a345f")

;; (set-cursor-color "cyan")
;; (set-cursor-color "gray")
;; (set-cursor-color "black")

;; (set-background-color "#f2f2f2")
;;; And this too
;; (set-background-color "#efefef")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Current
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

(dolist (theme-name '(subatomic256-theme
                      afternoon-theme
                      monokai-theme
                      solarized-theme
                      autumn-light-theme
                      tango-plus-theme
                      tommyh-theme))
  (unless (package-installed-p theme-name)
    (package-install theme-name)))

(~add-load-path "/m/src/emacs-themes")
(~add-load-path "/m/src/emacs-themes/tommyh-theme")
(~add-load-path "/m/src/emacs-themes/tango-plus-theme")

;; (require 'color-theme-acme-light)

;; (color-theme-textmate-modified)         ; Very good theme
(color-theme-acme-light)

;; (color-theme-molokai)
;; (color-theme-zenburn)
;; (color-theme-textmate)
;; (color-theme-twilight)
;; (inspiration-144382)
;; (inspiration-648409)
;; (inspiration-990434)
;; (color-theme-charcoal-black)
;; (color-theme-calm-forest)
;; (load-theme 'solarized-dark t)
;; (load-theme 'autumn-light t)
;; (load-theme 'tango-plus t)
;; (load-theme 'tommyh t)
;; (load-theme 'afternoon t)               ; Very good theme
;; (load-theme 'monokai t)         ; White color is too constrast that it hurt my
;;                                 ; eyes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Document me
(dolist (symb '(use-package))
  (put symb 'lisp-indent-function 1)
  (put symb 'common-lisp-indent-function 1)
  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,(~symbol->string symb) . font-lock-keyword-face))))

;; (custom-set-variables
;;  '(face-font-family-alternatives (quote (("Monaco" "Consolas" "Monospace")
;;                                          ("Monaco" "Consolas" "CMU Typewriter Text" "fixed")
;;                                          ("Geneva" "Sans Serif" "helv" "helvetica" "arial" "fixed")
;;                                          ("helv" "helvetica" "arial" "fixed"))))
;;  '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10) (encoding . utf-8))))
;;  '(show-paren-mode t)
;;  '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; (custom-set-faces
;;  '(default ((t (:inherit nil
;;                          :stipple nil
;;                          :inverse-video nil
;;                          :box nil
;;                          :strike-through nil
;;                          :overline nil
;;                          :underline nil
;;                          :slant normal
;;                          :weight normal
;;                          :height 100
;;                          :width normal
;;                          :foundry "unknown"
;;                          :family "Monaco"))))
;;  '(rst-level-1-face ((t (:embolden t))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg-specific UX configuration")

(provide 'ee:config-cmpitg-ux)
