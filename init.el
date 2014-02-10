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
;; Global variables
;;

(setq _config-dir (file-name-directory (or load-file-name
                                            (buffer-file-name))))

(load-file (concat _config-dir "init-global-vars.el"))
(load-file (concat _config-dir "init-essential-functions.el"))

(~start-emacs-server)

;;
;; Main code
;;

(~load-config-files "load-functions.el"
                    "init-package-manager.el"
                    "init-essential-packages.el"
                    "load-keymaps.el"
                    "load-packages.el"
                    "load-environment.el"
                    "load-menu.el"
                    "load-personal-stuff.el")
