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

;; TODO: Document me
(dolist (symb '(use-package))
  (put symb 'lisp-indent-function 1)
  (put symb 'common-lisp-indent-function 1)
  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,(~symbol->string symb) . font-lock-keyword-face))))

;;
;; Better Lisp indentation
;;
;; http://www.emacswiki.org/emacs/IndentingLisp
;;

;; (put 'if 'common-lisp-indent-function 1)
;; (put 'if 'common-lisp-indent-function 2)
