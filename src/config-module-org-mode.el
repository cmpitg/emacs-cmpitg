;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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

(use-package org
  :after (evil)
  :demand t
  :config
  (progn
    ;; Add timestamp when an item is done
    (setq org-log-done 'time)

    (setq org-agenda-files (thread-last (file-name-directory *toolbox-path*)
                             (f-glob "*.org")))

    ;; No folding by default
    ;; Ref: https://emacs.stackexchange.com/questions/9709/keep-the-headlines-expanded-in-org-mode
    ;; Of per file: #+STARTUP: showeverything
    (setq org-startup-folded nil)

    (with-eval-after-load "evil"
      (~bind-key-with-prefix "o a" #'org-agenda))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Org mode")

(provide 'rmacs:config-module-org-mode)
