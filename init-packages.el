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
;; Install all packages
;;

(apply #'elpa-install-packages   *elpa-packages*)
(apply #'el-get-install-packages *el-get-packages*)

;;; Load all recipes from `./recipes/` except for disabled recipes from
;;; `./recipes/disabled-packages.el`

(load "./recipes/disabled-packages.el")

(dolist (file-path (sort (directory-files "./recipes/") #'string<))
  (when (and (string-match "\\.el$" file-path)
             (not (memq (file-name-base file-path) *disabled-packages*)))
    (load (concat "./recipes/" file-path))))