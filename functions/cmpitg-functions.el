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

(defun -load-all-custom-functions ()
  "Load all custom functions in `*config-dir*/functions/`."
  (let* ((this-file-full-path (buffer-file-name))
         (this-file (concat (file-name-base this-file-full-path)
                            "."
                            (file-name-extension this-file-full-path))))
   (dolist (path (directory-files (-get-local-config-dir- "functions/")))
     (when (and (not (string-match (concat this-file "$") path)) ; Not this file
                (not (string-match "^[#$]" (file-name-base path))) ; Not backup
                                        ; file
                (string-match "\\.el$" path))
       (load path)))))

(provide 'cmpitg-functions)
