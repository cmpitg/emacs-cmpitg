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

;;
;; Slime for Common Lisp development
;;

(use-package slime
  :init (progn
          (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
          (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
          (setq inferior-lisp-program *default-lisp-repl-path*)
          (slime-setup)))
