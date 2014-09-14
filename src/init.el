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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *config-dir* (file-name-directory (or (buffer-file-name)
                                              load-file-name))
  ;; "/m/src/emacs-cmpitg-rewrite/src/"
  "Default configuration directory.")

(defvar *snippet-dir*
  (format "%s/snippets" *config-dir*)
  "Default snippet directory.")

(defvar *scratch-dir*
  "/m/scratch/"
  "Default path to Scratch directory.")

(add-to-list 'load-path *config-dir*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-config (&rest paths)
  "Returns path to a config file or directory."
  (apply 'concat *config-dir* paths))

(defun ~get-library-full-path (library-name)
  "Return the full path to a library."
  (save-excursion
    (find-library library-name)
    (let ((file-path (or (expand-file-name buffer-file-name)
                         "")))
      (kill-buffer)
      file-path)))

(defun ~elpa-install (&rest packages)
  "Install a list of packages with ELPA if not installed yet."
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ee:functions                (~get-config "functions.el"))
(require 'ee:load-package-manager     (~get-config "load-package-manager.el"))
(require 'ee:load-essential-packages  (~get-config "load-essential-packages.el"))
(require 'ee:custom-core              (~get-config "custom-core.el"))
(require 'ee:config-environment       (~get-config "environment.el"))
(require 'ee:keybindings              (~get-config "keybindings.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ee:cmpitg-flavored-packages (~get-config "cmpitg-flavored-packages.el"))
(require 'ee:personal                 (~get-config "personal.el"))
