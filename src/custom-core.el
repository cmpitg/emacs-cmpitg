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

(defvar *electrify-return-match*
  "[\]\)]"
  ;; "[\]}\)\"]"
  "If this regexp matches the text after the cursor, do an
\"electric\" return.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scroll one line at a time (less "jumpy" than defaults)
;; 3 lines at a time normally, 5 lines at a time with shift
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable Tramp autosave
(setq tramp-auto-save-directory "/tmp/")

;; Default scratch-buffer mode
;; (setq-default initial-major-mode 'emacs-lisp-mode)
(setq-default initial-major-mode 'adoc-mode)
(setq-default major-mode 'adoc-mode)

;; Don't let the cursor go into minibuffer prompt
;;   http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

;;; Use the same clipboard with X
(setq x-select-enable-clipboard t)

;; Disable shift selection
(setq shift-select-mode nil)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;; Always suggest-key-bindings
(setq suggest-key-bindings t)

;; More tolerable stack
(setq max-lisp-eval-depth 15000
      max-specpdl-size    15000)

;; Never change case when replacing
(setq-default case-replace nil)

;;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Set mark-ring-max
(setq mark-ring-max 512)

;; yes/no questions become y/n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backspace and Del delete selection, except in paredit-mode
(delete-selection-mode 1)

;; Set the default tab width
(setq default-tab-width 4)

;; Set tab width
(setq tab-width 4)

;; Default tab-size for C
(setq-default c-basic-offset 4)

;; Expand tabs to spaces
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preventing window stealing by cancelling the role of other-window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))

(ad-activate 'pop-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ee:custom-core)
