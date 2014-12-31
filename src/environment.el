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
;; Load all themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (theme-file (directory-files (~get-config "themes/")))
  (when (s-ends-with? ".el" theme-file)
    (load-file (~get-config "themes/" theme-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming and stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using system font

(setq font-use-system-font t)

;; (set-face-foreground 'font-lock-comment-face "#3a345f")
;;; Use this if you don't use any theme
(set-face-attribute 'font-lock-comment-face nil :foreground "#3a345f")

;; (set-cursor-color "cyan")
;; (set-cursor-color "gray")
;; (set-cursor-color "black")

;; (set-background-color "#f2f2f2")
(set-background-color "#efefef")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice he-substitute-string (after he-paredit-fix)
  "Remove extra paren when expanding line in Paredit"
  (when (and paredit-mode
             (equal (substring str -1) ")"))
    (backward-delete-char 1)
    (forward-char)))

(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (he-flexible-abbrev-collect he-search-string))))

  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))

  (cond ((null he-expand-list)
         (if old
             (he-reset-string))
         nil)
        (t
         (he-substitute-string (car he-expand-list))
         (setq he-expand-list (cdr he-expand-list))
         t)))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'symbol) collection))))
    collection))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
          "\\w*-*" "\\b"))

(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Document me
(dolist (symb '(use-package))
  (put symb 'lisp-indent-function 1)
  (put symb 'common-lisp-indent-function 1)
  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,(~symbol->string symb) . font-lock-keyword-face))))

;; Set ispell-dictionary
;; (ispell-change-dictionary "american")

;; grep command
(setq grep-command "grep -i -nH -e ")

;; Set printing type
(setq ps-paper-type 'a4)

;; Disable backup file
(setq make-backup-files nil)

;; Show column number
(column-number-mode 1)

;; Highlight the editing line
(hl-line-mode 1)

;; Turn on the search-highlighting feature
(setq search-highlight 1)

;; Case-insensitive searching
(setq-default case-fold-search t)
(setq case-fold-search t)

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; minibuffer window expands vertically as necessary to hold the text
;; that you put in the minibuffer
(setq resize-mini-windows t)

;; Never change case when replacing
(setq-default case-replace nil)

;; Pretty diff mode
(autoload 'ediff-buffers "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files "ediff" "Intelligent Emacs interface to diff" t)
(autoload 'ediff-files-remote "ediff"
  "Intelligent Emacs interface to diff")

;;
;; Log command, for making screencast
;;

;; Don't accelerate scrolling
;; (setq mouse-wheel-progressive-speed nil)

;; (setq scroll-step 3)

;; (setq scroll-conservatively 10000)

;; (setq redisplay-dont-pause t
;;       scroll-margin 1
;;       scroll-step 3
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)

;; (setq scroll-margin 1
;;       ;; scroll-conservatively 0
;;       scroll-conservatively 10000
;;       scroll-up-aggressively 0.01
;;       scroll-down-aggressively 0.01)
;; (setq-default scroll-up-aggressively 0.01
;;               scroll-down-aggressively 0.01)

;;
;; Recent files
;;

(setq recentf-max-menu-items 128)

(provide 'ee:config-environment)
