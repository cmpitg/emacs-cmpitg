;;
;; Copyright (C) 2017 Ha-Duong Nguyen (@cmpitg)
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

;;;
;;; For programmer Dvorak layout
;;;

(bind-key "<C-return>" '~eval-last-sexp-or-region emacs-lisp-mode-map)
(bind-key "<M-return>" 'eval-defun emacs-lisp-mode-map)
(bind-key "<S-return>" '~eval-current-sexp emacs-lisp-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg-specific keybindings")
(provide 'ee:keybindings-cmpitg)
