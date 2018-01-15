;; -*- no-byte-compile: t -*-

;;
;; Copyright (C) 2017-2018 Ha-Duong Nguyen (@cmpitg)
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

(load (concat (file-name-directory (or (buffer-file-name)
                                       load-file-name))
              "init-bare"))

(require 'rmacs:functions-cmpitg    "functions-cmpitg")
(require 'rmacs:config-themes       "config-themes")
(require 'ee:config-cmpitg-packages (~get-config "config-cmpitg-packages"))

(unless (string= "1" (getenv "EMACS_NO_EXPERIMENTAL"))
  (~load-files (~get-config "experimental")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine/user-specific config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load machine-specific settings if existed
(~load-files "~/.emacs-machine-specific" (~get-config "machine-specific"))
