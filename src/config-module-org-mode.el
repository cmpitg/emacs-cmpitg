;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018-2021 Ha-Duong Nguyen (@cmpitg)
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

(require 'misc)
(use-package org
  :demand t
  :straight nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show bullets
;; Ref: https://github.com/sabof/org-bullets
;; Ref: https://github.com/integral-dw/org-superstar-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-bullets
  :after (org)
  :config
  (progn
    (font-lock-add-keywords
     'org-mode
     '(("^ +\\([-*]\\) "
        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))))

(use-package org-superstar
  :after (org)
  :hook (org-mode . org-superstar-mode)
  :disabled t
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring Org mode")

(provide 'rmacs:config-module-org-mode)
