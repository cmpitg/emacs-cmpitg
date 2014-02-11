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

(defun ~generate-code-from-recipe (dir output-file)
  "Generate one big Emacs Lisp source file by merging all files
from `dir' directory (constructed by `~get-local-config-dir') in
alphabetical order into `output-file' for fast loading.

E.g.

;; Generate one file to load all functions
\(~generate-code-from-recipe \"recipes/functions/\" \"load-functions.el\"\)

;; Generate one file to load all packages
\(~generate-code-from-recipe \"recipes/packages/\" \"load-packages.el\"\)
"
  (save-excursion
    (with-temp-buffer
      (dolist (file (sort (~list-dir-full-path (~get-local-config-dir dir))
                          #'string<))
        (when (and (f-file? file)
                   (> (f-size file) 0)
                   (not (string-match "^\\#.*" file)))
          (insert-file file)
          (end-of-buffer)
          (insert-string "\n")))
      (write-file (~get-local-config-dir output-file)))))

;; (~generate-code-from-recipe "recipes/packages/" "recipe-packages.el")
;; (~generate-code-from-recipe "functions" "recipe-functions.el")

(~generate-code-from-recipe "recipes/functions/"   "load-functions.el")
(~generate-code-from-recipe "recipes/keymaps/"     "load-keymaps.el")
(~generate-code-from-recipe "recipes/menus/"       "load-menu.el")
(~generate-code-from-recipe "recipes/environment/" "load-environment.el")
