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

(use-package haskell-mode
  :init (progn
          (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
          (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
          ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
          ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
          ))
