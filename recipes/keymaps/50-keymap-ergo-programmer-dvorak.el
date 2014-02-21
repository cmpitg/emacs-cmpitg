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

(bind-key "s-k" 'clipboard-yank)
(bind-key "s-j" 'clipboard-kill-ring-save)
(bind-key "s-q" 'clipboard-kill-region)

(bind-key "s-K" '~delete-line)
(bind-key "s-l" 'goto-line)

(bind-key "s-c" 'previous-line)
(bind-key "s-t" 'next-line)
(bind-key "s-h" 'backward-char)
(bind-key "s-n" 'forward-char)

(use-package grizzl-read
  :config (progn
            (bind-key "s-c" 'grizzl-set-selection+1 *grizzl-keymap*)
            (bind-key "s-t" 'grizzl-set-selection-1 *grizzl-keymap*)))

;; (bind-key "s-d" 'move-beginning-of-line)
(bind-key "s-d" '~move-to-beginning-of-line)
(bind-key "C-a" '~move-to-beginning-of-line)
(bind-key "s-D" 'move-end-of-line)

(bind-key "M-s-c" '(lambda nil (interactive) (previous-line 5)))
(bind-key "M-s-t" '(lambda nil (interactive) (next-line 5)))
(bind-key "s-H" 'beginning-of-buffer)
(bind-key "s-N" 'end-of-buffer)

(bind-key "s-g" '(lambda ()
                  (interactive)
                  (if (~string-contains? (->string major-mode) "lisp")
                      (call-interactively 'backward-to-word)
                    (backward-sexp))))
(bind-key "s-G" 'backward-sexp)
;; (bind-key "s-r" 'forward-word)
(bind-key "s-r" (lambda ()
                  (interactive)
                  (if (~string-contains? (->string major-mode) "lisp")
                      (call-interactively 'forward-to-word)
                    (~smart-forward-exp))))
(bind-key "s-R" 'forward-sexp)

;;; Deleting

(bind-key "s-u" 'delete-char)
(bind-key "s-e" 'backward-delete-char)
(bind-key "s-p" 'kill-word)
(bind-key "s-." 'backward-kill-word)

(bind-key "s-x" (lambda ()
                  (interactive)
                  (kill-line)
                  (delete-horizontal-space)))
(bind-key "s-X" '~delete-line)

;;; Selection

(bind-key "s-_" '~mark-line)
(bind-key "s-)" '~mark-word)
(bind-key "s-S-SPC" '~mark-defun)

;;; Other

(bind-key "s--" 'comment-or-uncomment-region)
(bind-key "s-/" 'create-tags)
(bind-key "s-?" 'visit-tags-table)
(bind-key "s-f" 'query-replace-regexp)
(bind-key "s-F" 'query-replace)
(bind-key "s-j" 'clipboard-kill-ring-save)
;; (bind-key "<menu> m" 'toggle-menu-bar-mode-from-frame)

(bind-key "s-'" 'undo)
(bind-key "s-\"" 'undo-tree-redo)

;; (bind-key "s-s" 'isearch-forward-regexp)
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "s-s" 'helm-occur)
;; (bind-key "s-S" 'isearch-backward-regexp)

;;; With other libraries

(use-package paredit
  :config (progn
            (bind-key "s-." 'paredit-backward-kill-word paredit-mode-map)
            (bind-key "s-p" 'paredit-forward-kill-word paredit-mode-map)))

(use-package helm
  :config (progn
            (bind-key "s-t" 'helm-next-line helm-map)
            (bind-key "s-c" 'helm-previous-line helm-map)))
