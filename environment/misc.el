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

;; Disable shift selection
(setq shift-select-mode nil)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;; Always suggest-key-bindings
(setq suggest-key-bindings t)

;; Set ispell-dictionary
(ispell-change-dictionary "en_US")

;; grep command
(setq grep-command "grep -i -nH -e ")

;; Set printing type
(setq ps-paper-type 'a4)

;;; Use the same clipboard with X
(setq x-select-enable-clipboard t)

;;; Auto complete switching buffer mode
(iswitchb-mode t)

;; Disable backup file
(setq make-backup-files nil)

;;; Auto complete switching buffer mode
(iswitchb-mode t)

;; Default scratch-buffer mode
(setq initial-major-mode 'emacs-lisp-mode)
(setq-default initial-major-mode 'emacs-lisp-mode)

;; Don't let the cursor go into minibuffer prompt
;;   http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

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

;; Make shebang-ed files executable
(add-hook 'after-save-hook '~make-executable)
