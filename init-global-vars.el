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

(defvar *elpa-package-list* 
  '(jedi                                ; Autocomplete and cool stuff for
                                        ; Python development
    ruby-mode
    ruby-dev                            ; Ruby and Pry
    yaml-mode
    go-mode
    haskell-mode
    markdown-mode
    markdown-mode+
    sr-speedbar                         ; Speedbar in the same frame
    paredit                             ; Parentheses manipulation in Lisp
                                        ; modes
    smartparens                         ; Auto-pairing parentheses
    auto-complete
    slime                               ; Best Common Lisp development tool
    openwith                            ; Open file with external program, in
                                        ; any file-browsing modes like Sunrise
                                        ; Commander or Dired
    yasnippet                           ; Textmate-like snippet and better
    gist                                ; Interface to Github's gist
    json                                ; JSON lib for Emacs
    js2-mode                            ; JavaScript
    pabbrev                             ; Better abbrev
    dired-details+                      ; Better Dired information control
    magit                               ; Best Git interface for Emacs
    monky                               ; Magit-like mode for Mercurial
    zlc                                 ; Zsh completion
    hexrgb                              ; Color manipulation
    flx-ido                             ; Better ido
    projectile                          ; Better project management
    helm-projectile                     ; Projectile-Helm integration
    grizzl                              ; Grizzl fuzzy matching method

    ;; ack-and-a-half ; -> Failed to require when installed with Elpa
    wgrep-ack                           ; Edittable Ack
    )
  "List of packages using in this Emacs configuration.")

(defvar *el-get-package-list*
  '(later-do                            ; Async eval
    powerline                           ; @johnathanchu version, beautiful
                                        ; modeline
                                        ; https://github.com/jonathanchu/emacs-powerline
    multi-scratch                       ; Multiple scratch buffers
    moz-repl                            ; MozRepl
    whitespace                          ; Display trailing whitespace
    )
  "List of packages not available in ELPA but available to install with el-get.")

(defvar *local-package-list*
  '(ibus                                ; iBus interface
    picolisp
    ack-and-a-half                      ; Ack - a better grep
    ))

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
      *elpa-package-dir*       "~/.emacs.d/elpa/"
      *el-get-package-dir*     "~/.emacs.d/el-get/"
      *license-dir*            "~/emacs-config/license-list"
      *me*                     "Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))")
