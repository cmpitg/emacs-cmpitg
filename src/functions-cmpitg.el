;;  -*- lexical-binding: t; -*-

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

(defun ~jekyll-add-last-updated ()
  "Adds `last_update' timestamp with `date -R` format for
Jekyll."
  (interactive)
  (save-excursion
    (goto-point (point-min))
    (if (re-search-forward "^last_updated:.*$")
        (replace-match (format "last_updated: %s"
                               (string-trim (~exec "date -R")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg-specific functions ")

(provide 'rmacs:functions-cmpitg)
