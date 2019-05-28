;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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

(require 'cl)

(if (string= "1" (getenv "EMACS_FORCE_TOGGLE_DEBUG_ON_ERROR"))
    (setq debug-on-error t)
  (setq debug-on-error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important global values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *config-dir* (file-name-directory (or (buffer-file-name)
                                                load-file-name))
  "Default configuration directory - the one containing this file.")

(defvar *snippet-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "snippets"))
  "Default snippet directory.")

(defvar *scratch-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "scratch"))
  "Default path to Scratch directory.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-config (&rest paths)
  "Path to a config file or directory."
  (apply 'concat *config-dir* paths))

(defun ~load-files (&rest paths)
  "Loads Emacs Lisp source files when they exist."
  (dolist (file-path paths)
    (loop for possible-file-path in (list file-path
                                          (concat file-path ".el")
                                          (concat file-path ".elc"))
          when (and (file-exists-p possible-file-path)
                    (file-regular-p possible-file-path))
          do (progn (load file-path)
                    (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "
%s
Invoke debugger when error: %s
Server name: %s
"
         (emacs-version)
         debug-on-error
         server-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On to configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path *config-dir*)

(~load-files "~/.emacs-machine-specific-init"
             (~get-config "machine-specific-init"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished bootstrapping configuration")

(provide 'rmacs:bootstrap-config)
