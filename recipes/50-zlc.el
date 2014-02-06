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
;; zlc - Zsh completion
;;

(use-package zlc
  :init (progn
          (zlc-mode t)
          (define-key minibuffer-local-map (kbd "<down>")  'zlc-select-next-vertical)
          (define-key minibuffer-local-map (kbd "<up>")    'zlc-select-previous-vertical)
          (define-key minibuffer-local-map (kbd "<right>") 'zlc-select-next)
          (define-key minibuffer-local-map (kbd "<left>")  'zlc-select-previous)
          (define-key minibuffer-local-map (kbd "C-c")     'zlc-reset)))
