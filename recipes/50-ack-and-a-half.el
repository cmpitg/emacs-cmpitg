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

(load (-get-local-config-dir- "local-packages/ack-and-a-half/ack-and-a-half.el"))

(use-package ack-and-a-half
  :init (progn
          ;; Fix Debian-based distros' executable file
          (setq ack-and-a-half-executable (or (executable-find "ack-grep")
                                              (executable-find "ack")))
          (defalias 'ack 'ack-and-a-half)
          (defalias 'ack-same 'ack-and-a-half-same)
          (defalias 'ack-find-file 'ack-and-a-half-find-file)
          (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))
