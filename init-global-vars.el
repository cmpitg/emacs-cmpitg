;;
;; Copyright (C) 2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
;;
;; This project is free softwsare: you can redistribute it and/or modify it
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
    ;; icicles
    dired+
    desktop                             ; Keeping workspace
    tar-mode
    saveplace
    color-theme
    smooth-scrolling
    flx-ido                             ; Better ido
    smartscan                           ; Jump between occurrences of a symbol
    smex                                ; Better M-x
    fiplr                               ; Find file with fuzzy matching
    use-package                         ; Great way to load packages
    ;; ack-and-a-half ; -> Failed to require when installed with Elpa
    wgrep-ack                           ; Edittable Ack
    browse-kill-ring
    tabbar-ruler
    )
  "List of packages that are vital to this config and must be
installed and loaded.")

(defvar *elpa-packages*
  '(dired-single                        ; Reuse dired buffer when opening
    dired-details+
    jedi                                ; Autocomplete and cool stuff for
                                        ; Python development
    virtualenvwrapper                   ; Virtualenvwrapper interface for
                                        ; Emacs
    ruby-mode
    ruby-dev                            ; Ruby and Pry
    yaml-mode
    go-mode
    haskell-mode
    markdown-mode
    markdown-mode+
    ace-jump-mode                       ; Quick jumping
    ;; sr-speedbar                         ; Speedbar in the same frame
    paredit                             ; Parentheses manipulation in Lisp
                                        ; modes
    smartparens                         ; Auto-pairing parentheses
    auto-complete
    slime                               ; Best Common Lisp development tool
    yasnippet                           ; Textmate-like snippet and better
    gist                                ; Interface to Github's gist
    json                                ; JSON lib for Emacs
    js2-mode                            ; JavaScript
    pabbrev                             ; Better abbrev
    magit                               ; Best Git interface for Emacs
    git-gutter-fringe                   ; Sublime Text-inspired in-buffer Git
                                        ; diff
    ahg                                 ; Better mode for Mercurial
    zlc                                 ; Zsh completion
    hexrgb                              ; Color manipulation
    projectile                          ; Better project management
    helm-projectile                     ; Projectile-Helm integration
    grizzl                              ; Grizzl fuzzy matching method

    request                             ; HTTP request library
    httprepl                            ; HTTP REPL

    swoop                               ; Great buffer navigation with
                                        ; pattern searching

    highlight-parentheses
    scratch-ext                         ; Better way to manage scratch buffer

    ;; For Clojure development
    clojure-mode
    clojure-cheatsheet
    clojure-test-mode
    cider
    clojurescript-mode
    ;; clojure-lint-mode
    ac-nrepl

    ;; sunrise-commander                   ; File manager
    ;; sunrise-x-tabs
    ;; sunrise-x-loop                      ; Execute commands async in SR
    ;; sunrise-x-checkpoints
    ;; sunrise-x-tree                      ; Tree browsing feature for SR
    ;; sunrise-x-modeline                  ; Nicer modeline in SR

    geiser                              ; Scheme development
    ac-geiser                           ; Auto complete backend for Geiser
    quack                               ; Another mode for Scheme development,
                                        ; load *after* Geiser

    diminish                            ; Hide some minor mode

    menu-bar+
    exec-path-from-shell
    w3m
    hyde                                ; Jekyll blogging management
    url-shortener
    json-mode)
  "List of packages using in this Emacs configuration.")

(defvar *el-get-packages*
  '(later-do                            ; Async eval
    multi-scratch                       ; Multiple scratch buffers
    moz-repl                            ; MozRepl
    whitespace                          ; Display trailing whitespace
    hs-lint                             ; Haskell linter
    json-mode                           ; For en/decoding JSON
    )
  "List of packages not available in ELPA but available to install with el-get.")

(defvar *local-packages*
  '(ibus                                ; iBus interface
    picolisp
    ack-and-a-half                      ; Ack - a better grep
    ))

(setq openwith-associations
      '(("\\.pdf\\'" "evince" (file))
        ("\\.djvu\\'" "evince" (file))
        ("\\.mp3\\'" "smplayer" (file))
        ("\\.avi\\'" "smplayer" (file))
        ("\\.mp4\\'" "smplayer" (file))
        ("\\.flv\\'" "smplayer" (file))
        ("\\.odt\\'" "libreoffice" (file))
        ("\\.odp\\'" "libreoffice" (file))
        ("\\.ppt\\'" "libreoffice" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\|m4v\\)\\'" "smplayer" (file))
        ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file))))

(setq *custom-els-dir*         (concat *config-dir* "/config-default")
      *snippet-dir*            (concat *config-dir* "/snippets")
      *license-dir*            (concat *config-dir* "/license-list")
      *saved-macro-path*       (concat *config-dir* "/functions/saved-macros.el")
      *scratch-dir*            "/m/scratch/"

      *ctags-path*             "/usr/bin/ctags-exuberant"
      *default-lisp-repl-path* (expand-file-name "~/bin/sbcl")
      *elpa-package-dir*       "~/.emacs.d/elpa/"
      *el-get-package-dir*     "~/.emacs.d/el-get/"

      *me*                     "Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))")

(defvar *electrify-return-match*
  "[\]\)]"
  ;; "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an
\"electric\" return.")
