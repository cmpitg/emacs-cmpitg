;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2014-2020 Ha-Duong Nguyen (@cmpitg)
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
;; Theming
;;

(defun ~set-attribute-all-frames (key val)
  "Sets attribute for all frames."
  (put 'initial-frame-alist key val) ;; for all frames
  (put 'default-frame-alist key val)
  (modify-frame-parameters (selected-frame) (list (cons key val))))

;; Use this if you don't use any theme
;; (set-face-foreground 'font-lock-comment-face "#3a345f")
;; (set-background-color "#f6f6f6")
;; (set-background-color "#efefef")
(set-background-color "#ffffdd")
(~set-attribute-all-frames 'background-color "#ffffdd")
(~set-attribute-all-frames 'foreground-color "black")
(set-face-attribute 'cursor nil
                    :background "#0f0f0f")

(add-to-list 'custom-theme-load-path (~get-config "themes/"))
;; (~load-files (~get-config "themes/cmpitg-thursday-theme.el"))
;; (load-theme 'cmpitg-thursday t)
;; (load-theme 'cmpitg-random-light t)

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi))
;; OR (modus-themes-load-vivendi))

;; Current
;; (load-theme 'plan9 t)
;; (load-theme 'afternoon t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring themes")

(provide 'rmacs:config-themes)
