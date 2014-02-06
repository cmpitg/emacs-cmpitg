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

(use-package tabbar-ruler
  :init (progn
          (setq tabbar-ruler-global-tabbar t)    ; If you want tabbar
          (setq tabbar-ruler-global-ruler t)     ; if you want a global ruler
          (setq tabbar-ruler-popup-menu t)       ; If you want a popup menu.
          (setq tabbar-ruler-popup-toolbar t)    ; If you want a popup toolbar
          (setq tabbar-ruler-popup-scrollbar t)) ; If you want to only show
                                                 ; the scroll bar when your
                                                 ; mouse is moving.
  )
