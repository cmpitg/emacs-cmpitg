;;
;; Copyright (C) 2014-2017 Ha-Duong Nguyen (@cmpitg)
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

;; Load init-basic
(load (concat (file-name-directory (or (buffer-file-name)
                                       load-file-name))
              "init-basic"))

(require 'ee:functions-mail         (~get-config "functions-mail"))
(require 'ee:functions-cmpitg       (~get-config "functions-cmpitg"))
(require 'ee:config-cmpitg-ux       (~get-config "config-cmpitg-ux"))
(require 'ee:keybindings-cmpitg     (~get-config "keybindings-cmpitg"))
(require 'ee:config-mail            (~get-config "config-mail"))

(unless (string= "1" (getenv "EMACS_NO_EXPERIMENTAL"))
  (~load-files (~get-config "experimental")))

;; Load last
(require 'ee:config-final (~get-config "config-final"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine/user-specific config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load machine-specific settings if existed
(~load-files "~/.emacs-machine-specific" (~get-config "machine-specific"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading Emacs mail")
