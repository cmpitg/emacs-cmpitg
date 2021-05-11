;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018-2020 Ha-Duong Nguyen (@cmpitg)
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
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package f)
(require 'misc)
(use-package org
  :init (progn
          (require 'org-archive)
          (defun ~org-set-fold-entry ()
            (interactive)
            (org-set-property "~fold?" "true"))

          (defun ~org-is-entry-folded? (&optional pom)
            (or (string= (org-entry-get pom "~fold?") "true")
                (ignore-errors (org-entry-is-done-p))))

          (defun ~org-fold-entry ()
            (interactive)
            (org-flag-subtree t))

          (defun ~org-unfold-entry ()
            (interactive)
            (org-flag-subtree nil))

          (defun ~org-unfold-all ()
            (interactive)
            (org-show-all '(headings block)))

          (defun ~org-refresh-fold-state ()
            (interactive)
            (save-excursion
              (beginning-of-buffer)
              (while (not (eobp))
                (when (~org-is-entry-folded?)
                  (~org-fold-entry))
                (~org-to-next-entry))))

          (defalias '~org-to-next-entry #'outline-next-heading)
          (defalias '~org-to-prev-entry #'outline-previous-heading)

          (defun ~my/org-mode-setup ()
            (bind-key "<S-return>" #'~execute-line org-mode-map)
            (bind-key "<C-return>" #'~eval-last-sexp-or-region org-mode-map)
            (bind-key "C-<tab>" #'iflipb-next-buffer org-mode-map)
            (bind-key "C-S-<tab>" #'iflipb-previous-buffer org-mode-map)
            (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer org-mode-map)
            (bind-key "C-e" nil org-mode-map)
            (~org-refresh-fold-state)
            (font-lock-mode t))

          ;; Add timestamp when an item is done
          (setq org-log-done 'time)

          (setq org-agenda-files (thread-last (file-name-directory *toolbox-path*)
                                   (f-glob "*.org")))

          ;; Indent visually by default
          (setq org-startup-indented t)

          ;; Smart editing of invisible text
          (setq org-catch-invisible-edits 'smart)

          ;; TAB-cycle plain list as children of their heading parent
          (setq org-cycle-include-plain-lists 'integrate)

          ;; Continuation symbol
          (setq org-ellipsis " ↩")

          ;; Don't split line by default
          (setq org-M-RET-may-split-line nil)

          ;; Don't fontify code block by default
          (setq org-src-fontify-natively nil)

          ;; Preserve indentation in org-src
          (setq org-src-preserve-indentation t)

          ;; Enable shift-selection all the time
          (setq org-support-shift-select 'always)

          ;; Logical TODO & checkbox dependencies
          (setq org-enforce-todo-dependencies t)
          (setq org-enforce-todo-checkbox-dependencies t)

          ;; No folding by default
          ;; Ref: https://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
          ;; Of per file: #+STARTUP: all
          (setq org-startup-folded nil)

          (add-hook 'org-mode-hook #'~my/org-mode-setup)

          (setq-default initial-major-mode 'org-mode)
          (setq-default major-mode 'org-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Babel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(setq org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)
        (R . t)
        (clojure . t)))
(org-babel-do-load-languages 'org-babel-load-languages
                             org-babel-load-languages)
(setq org-confirm-babel-evaluate nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Org mode")

(provide 'rmacs:config-module-org-mode)
