;;
;; Copyright (C) 2017 Ha-Duong Nguyen (@cmpitg)
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

(defun ~visit-toolbox ()
  "Visits my toolbox."
  (interactive)
  (~open-file-if-existed (or *toolbox-path*
                             "/m/Toolbox/Toolbox.md")))

(defun ~visit-keybindings ()
  "Jump to personal keybinding file."
  (interactive)
  (~open-file-if-existed (~get-config "keybindings.el")))

(defun ~visit-experimental-config ()
  "Visits the experimental config."
  (interactive)
  (~open-file-if-existed (~get-config "experimental.el")))

(defun ~visit-cmpitg-package-config ()
  "Visit my personal config."
  (interactive)
  (~open-file-if-existed (~get-config "cmpitg-flavored-packages.el")))

(defun ~add-load-path (path)
  "Add path to load-path."
  (add-to-list 'load-path path))

(defun ~jekyll-add-last-updated ()
  "Add last_update timestamp with `date -R` format."
  (interactive)
  (save-excursion
    (goto-point (point-min))
    (if (re-search-forward "^last_updated:.*$")
        (replace-match (format "last_updated: %s"
                               (~string-but-last (~exec "date -R")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg helpers")
(provide 'ee:functions-cmpitg)
