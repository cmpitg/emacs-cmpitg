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
(use-package org)
(defun ~my/org-mode-setup ()
  (bind-key "<S-return>" #'~execute-line org-mode-map)
  (bind-key "<C-return>" #'~eval-last-sexp-or-region org-mode-map)
  (bind-key "C-<tab>" #'iflipb-next-buffer org-mode-map)
  (bind-key "C-S-<tab>" #'iflipb-previous-buffer org-mode-map)
  (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer org-mode-map)
  (bind-key "C-e" nil org-mode-map)
  (font-lock-mode -1))

;; Add timestamp when an item is done
(setq org-log-done 'time)

(setq org-agenda-files (thread-last (file-name-directory *toolbox-path*)
                         (f-glob "*.org")))

(setq org-startup-indented t)

;; No folding by default
;; Ref: https://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
;; Of per file: #+STARTUP: all
(setq org-startup-folded nil)

(add-hook 'org-mode-hook #'~my/org-mode-setup)

(setq-default initial-major-mode 'org-mode)
(setq-default major-mode 'org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Babel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . nil)
                               (python . t)
                               (R . t)
                               (clojure . t)))
(setq org-confirm-babel-evaluate nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Org mode")

(provide 'rmacs:config-module-org-mode)
