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

(defvar *config-dir* (expand-file-name "~/emacs-config")
  "Path to main config directory.")

(defun -load-files-if-exists- (&rest paths)
  "Load files when they exists."
  (dolist (file-path paths)
   (when (file-exists-p file-path)
     (load file-path))))

(defun -get-local-config-dir- (feature)
  "Return local config directory for a feature.  This function
does nothing more than concat-ing `*config-dir' with `feature'."
  (format "%s/%s" *config-dir* feature))

(add-to-list 'load-path *config-dir*)
(add-to-list 'load-path (-get-local-config-dir- "functions"))

(require 'cmpitg-functions)

;; (-load-files-if-exists- "~/emacs-config/package-list.el"
;;                         "~/emacs-custom-foremost.el" ; User-defined
;;                         "~/emacs-config/global-vars.el"
;;                         "~/emacs-config/init-package-manager.el"
;;                         "~/emacs-config/main.el"
;;                         "~/emacs-config/config-default/environment.el"
;;                         "~/emacs-custom.el"          ; User-defined
;;                         )
