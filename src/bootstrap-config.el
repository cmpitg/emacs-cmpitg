;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2018-2020 Ha-Duong Nguyen (@cmpitg)
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

(require 'cl-lib)

(setq init-file-user (user-login-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important global values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *config-dir* (concat (getenv "RMACS_INIT_DIR") "/src/")
  "Default configuration directory - the one containing this file.")

(defvar *snippet-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "snippets"))
  "Default snippet directory.")

(defvar *scratch-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "scratch"))
  "Default path to Scratch directory.")

(defvar *popup-buffer-in*
  :window
  "Determines whether a buffer popped up by `~popup-buffer' is in
a new window or a new frame.  Possible values: `:window',
`:frame'.")

(defvar *~exec-history-path*
  (expand-file-name (format "~/.local/rmacs.%s.exec-history"
                            (if (boundp 'server-name)
                                server-name
                              "default")))
  "Path to history file that stores executed external commands")
(defvar *~exec-history-max* 3000
  "How many entries are saved in `*~EXEC-HISTORY-PATH*'")

(defvar *~output-beginning-marker* "### ««« ###"
  "String that marks the beginning of the output from the interpreter.")
(defvar *~output-end-marker* "### »»» ###"
  "String that marks the end of the output from the interpreter.")

(defvar *~move-to-destination-after-exec?* t
  "Determines if the cursor is moved to a destination after exec'ing.")

(defvar *~marker-regexp* (rx bol (0+ " ")
                             (or "----" "mux://" "$" "!!!" "!!" "!@" "!^" "!"
                                 (eval *~output-beginning-marker*)
                                 (eval *~output-end-marker*)))
  "Regexp that determines visual markers.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-config (&rest paths)
  "Path to a config file or directory."
  (apply 'concat *config-dir* paths))

(defun ~load-files (&rest paths)
  "Loads Emacs Lisp files when they exist."
  (dolist (file-path paths)
    (load file-path t nil)))

(defun ~reload-config ()
  "Reloads Rmacs config"
  (interactive)
  (~load-files (~get-config "init-" (substring (symbol-name *rmacs-shape*) 1))))

(defun ~get-default-monospace-font ()
  "Gets the default monospace font."
  (cond
   ((x-list-fonts "Cascadia Mono") '(:font "Cascadia Mono"))
   ((x-list-fonts "Fira Code") '(:font "Fira Code"))
   ((x-list-fonts "Noto Sans Mono") '(:font "Noto Sans Mono"))
   ((x-list-fonts "Open Sans Mono") '(:font "Open Sans Mono"))
   ((x-family-fonts "Roboto") '(:family "Roboto"))))

(defun ~get-default-font ()
  "Gets the default font."
  (cond
   ((x-list-fonts "Roboto") '(:font "Roboto"))
   ((x-family-fonts "Roboto") '(:family "Roboto"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging current Rmacs information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "%s :: debug-on-error=%s server-name=%s init-file-user=%s\n"
         (emacs-version)
         debug-on-error
         (if (boundp 'server-name)
             server-name
           "default")
         init-file-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On to configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use straight.el, not default package manager
;; Ref: https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
(setq package-enable-at-startup nil)

(add-to-list 'load-path *config-dir*)

(~load-files "~/.emacs-machine-specific-init"
             (~get-config "machine-specific-init"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished bootstrapping configuration")

(provide 'rmacs:bootstrap-config)
