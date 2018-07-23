;; -*- no-byte-compile: t -*-

;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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

;; Load bare Rmacs
(load (concat (file-name-directory (or (buffer-file-name)
                                       load-file-name))
              "init-bare"))

(require 'rmacs:functions-aws                "functions-aws")
(require 'rmacs:functions-cmpitg             "functions-cmpitg")
(require 'rmacs:config-literate-prog         "config-literate-prog")
(require 'rmacs:config-themes                "config-themes")
(require 'rmacs:config-edit                  "config-edit")

(unless (string= "1" (getenv "EMACS_NO_EXPERIMENTAL"))
  (~load-files (~get-config "experimental")))

;; Machine/user-specific config
(~load-files "~/.emacs-machine-specific" (~get-config "machine-specific"))

(message "Finish loading Rmacs edit!")
