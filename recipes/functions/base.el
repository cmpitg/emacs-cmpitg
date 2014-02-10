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

(require 'cl)

(defun ~helm-grep ()
  "C-u helm-do-grep"
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'helm-do-grep)))

(defun toolbox:open-file (path)
  "Open path and open with external program if necessary."
  (condition-case description
      (find-file path)))

(defun toolbox:execute-and-replace ()
  "Execute command on selection using `wand:execute' then replace
selection with command output."
  (interactive)
  (let* ((text (save-excursion
                (get-selection)))
         (output (wand:execute text)))
    (call-interactively 'kill-region)
    (insert output)))

;;;
;;; TODO: Document me
;;;

;; (defvar *$emacs-lisp-keywords*
;;   '("defalias")
;;   "List of symbols we want to treat as keywords.")

;; (defvar *$emacs-lisp-functions*
;;   '("~auto-load-mode")
;;   "List of symbols we want to treat as \"special\" functions.")

;; (put 'font-lock-add-keywords 'lisp-indent-function 1)

;; ;;; Better `if' and `list' indentation
;; (put 'list 'lisp-indent-function nil)
;; (put 'if 'lisp-indent-function 1)
;; (put 'quote lisp-indent-function 1)

;; (font-lock-add-keywords 'emacs-lisp-mode
;;   (list (cons (eval-when-compile
;; 		(regexp-opt *$emacs-lisp-keywords* 'words))
;; 	      font-lock-keyword-face)
;; 	(cons (eval-when-compile
;; 		(regexp-opt *$emacs-lisp-functions* 'words))
;; 	      font-lock-function-name-face)))


(defalias 'qrr 'query-replace-regexp)
(defalias 'sr 'search-forward-regexp)
(defalias 'vtt 'visit-tags-table)
(defalias 'cr 'create-tags)
(defalias 'ib 'ibus-mode)
(defalias 'rb 'revert-buffer)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'tw 'twit)
(defalias 'bs 'bookmark-save)
(defalias 'am 'auto-complete-mode)
(defalias 'fm 'folding-mode)
