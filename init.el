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

(setq _current-dir (file-name-directory (or load-file-name
                                            (buffer-file-name))))

(load-file (concat _current-dir "init-global-vars.el"))

;;
;; Essential functions, used to load other things
;;

(defun ~load-files (&rest paths)
  "Load files when they exists."
  (dolist (file-path paths)
    (when (and (file-exists-p file-path)
               (file-regular-p file-path))
      (load-file file-path))))

(defun ~load-config-files (&rest paths)
  "Load files when they exists."
  (apply #'~load-files (mapcar (lambda (path)
                                 (~get-local-config-dir path))
                               paths)))

(defun ~get-local-config-dir (feature)
  "Return local config directory for a feature.  This function
does nothing more than concat-ing `*config-dir' with `feature'."
  (format "%s/%s" *config-dir* feature))

;;
;; Main code
;;

(~load-config-files "functions/cmpitg-functions.el")
(~load-all-custom-functions)

;; TODO: Document me
(put 'use-package 'lisp-indent-function 1)
(font-lock-add-keywords 'emacs-lisp-mode
  '(("use-package" . font-lock-keyword-face)))

(start-emacs-server)

(~load-config-files "init-package-manager.el"
                    "init-essential-packages.el"
                    "init-packages.el"
                    "init-environment.el"
                    "init-menu.el")
