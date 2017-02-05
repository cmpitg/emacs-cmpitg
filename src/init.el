;;
;; Copyright (C) 2014-2016 Ha-Duong Nguyen (@cmpitg)
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
;; Important global values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *config-dir* (file-name-directory (or (buffer-file-name)
                                                load-file-name))
  "Default configuration directory.")

(defvar *snippet-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "snippets"))
  "Default snippet directory.")

(defvar *scratch-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "scratch"))
  "Default path to Scratch directory.")

(add-to-list 'load-path *config-dir*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialized Emacs - Mail browser, ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun ~value-from-symbol (symbol)
  "Returns the value that `symbol' hold if it's bound, or `nil'
  otherwise."
  (if (boundp symbol)
      (symbol-value symbol)
    nil))

;; Prior to Emacs 25, `reverse' doesn't receive string argument
(defun _reverse-string (str)
  "Reverse a string."
  (apply #'string (reverse (string-to-list str))))

(defun cmpitg/emacs-as ()
  "Return :mail, :notes, or nil when Emacs is running as mail
browser, note taker, or ... just Emacs."
  (loop for x in process-environment
        when (and (string-prefix-p "EMACS_ENABLED_" x nil)
                  (string-prefix-p "1=" (_reverse-string x)))
        return (let ((x (substring x (length "EMACS_ENABLED_"))))
                 (intern (concat ":" (replace-regexp-in-string
                                      "_"
                                      "-"
                                      (downcase
                                       (substring x
                                                  0 (- (length x)
                                                       (length "1="))))))))))

(defun cmpitg/specialized-emacs? ()
  "Check if Emacs is running in specialized mode (mail browser,
note taker, ...)"
  (cmpitg/emacs-as))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
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
       file-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string= "1" (getenv "EMACS_FORCE_TOGGLE_DEBUG_ON_ERROR"))
    (setq debug-on-error t)
   (setq debug-on-error nil))

(require 'ee:functions                (~get-config "functions.el"))

;; Load machine-specific-init settings if existed
(~load-files "~/.emacs-machine-specific-init.el"
             (~get-config "machine-specific-init.el"))

(require 'ee:load-package-manager     (~get-config "load-package-manager.el"))
(require 'ee:load-essential-packages  (~get-config "load-essential-packages.el"))
(require 'ee:custom-core              (~get-config "custom-core.el"))
(require 'ee:config-environment       (~get-config "environment.el"))
(require 'ee:keybindings              (~get-config "keybindings.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ee:cmpitg-flavored-packages
         (~get-config "cmpitg-flavored-packages.el"))

(unless (string= "1" (getenv "EMACS_NO_EXPERIMENTAL"))
  (~load-files (~get-config "experimental.el")))

;; Load machine-specific settings if existed
(~load-files "~/.emacs-machine-specific.el"
             (~get-config "machine-specific.el"))
