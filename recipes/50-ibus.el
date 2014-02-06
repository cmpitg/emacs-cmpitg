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

(use-package ibus
  :init (progn
          ;; Use C-SPC for Set Mark command
          (ibus-define-common-key ?\C-\s nil)
          ;; Use C-/ for Undo command
          (ibus-define-common-key ?\C-/ nil)
          ;; Change cursor color depending on IBus status
          (setq ibus-cursor-color '("red" "blue" "limegreen"))
          (setq ibus-agent-file-name "~/emacs-config/emacs-local-packages/ibus.el/ibus-el-agent")))
