;; -*- no-byte-compile: t -*-

;;
;; Copyright (C) 2014-2022 Ha-Duong Nguyen (@cmpitg)
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

(load (concat (file-name-directory (or load-file-name (buffer-file-name))) "init-minimal"))

(require 'rmacs:functions-mail    "functions-mail")
(require 'rmacs:functions-cmpitg  "functions-cmpitg")
(require 'rmacs:config-mail       "config-mail")

(unless (string= "1" (getenv "EMACS_NO_EXPERIMENTAL"))
  (~load-files (~get-config "experimental")))

;; Machine/user-specific config
(~load-files "~/.emacs-machine-specific" (~get-config "machine-specific"))

(message "Finish loading Rmacs mail")
