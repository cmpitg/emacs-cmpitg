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


(defvar *config-dir* (expand-file-name "~/emacs-config")
  "Path to main config directory.")

(defvar *essential-packages*
  '(dash                                ; "Modern" list processing
    ht                                  ; The missing hashtable library
    s                                   ; "Modern" string processing
    f                                   ; "Modern" file APIs
    cl                                  ; Common Lisp subset in Emacs Lisp
    cl-lib                              ; Common Lisp library
    helm                                ; Smart completion framework
    thingatpt                           ; Getting thing at current pointg
    multiple-cursors                    ; Sublime-like multiple cursors
    expand-region                       ; Expand selection based-on semantic
                                        ; units
    eldoc                               ; Echo area function signature
    popwin                              ; Better popwin window management,
                                        ; dispose with Esc or C-g
    color-theme
    smooth-scrolling
    smartscan                           ; Jump between occurrences of a symbol
    smex                                ; Better M-x
    fiplr                               ; Find file with fuzzy matching
    )
  "List of packages that are vital to this config and must be
installed and loaded.")

(setq openwith-associations
  '(("\\.pdf\\'" "evince" (file))
    ("\\.mp3\\'" "smplayer" (file))
    ("\\.odt\\'" "libreoffice" (file))
    ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|m4v\\)\\'" "smplayer" (file))
    ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))))

(setq *custom-functions-path*  "~/emacs-config/config-default/custom-functions.el"
      *snippet-dir*            "~/emacs-config/snippets"
      *custom-els-dir*         "~/emacs-config/config-default/"
      *default-lisp-repl-path* (expand-file-name "~/bin/sbcl")
      *elpa-package-dir*       "~/.emacs.d/elpa/")
