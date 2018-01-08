;; -*- no-byte-compile: t -*-

;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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

(if (string= "1" (getenv "EMACS_FORCE_TOGGLE_DEBUG_ON_ERROR"))
    (setq debug-on-error t)
  (setq debug-on-error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important global values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *config-dir* (file-name-directory (or (buffer-file-name)
                                                load-file-name))
  "Default configuration directory - the one containing this file")

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
  "Return the value that `symbol' hold if it's bound, or `nil'
otherwise."
  (if (boundp symbol)
      (symbol-value symbol)
    nil))

;; Prior to Emacs 25, `reverse' doesn't receive string argument
(defun _reverse-string (str)
  "Reverse a string."
  (apply #'string (reverse (string-to-list str))))

(defvar *emacs-as-tool* nil
  "Determine which shape Emacs is running as.")

(defun ~emacs-as ()
  "Return `:mail', `:notes', :file-browser or `nil' when Emacs is
running as mail browser, note taker, or ... just Emacs the text editor."
  *emacs-as-tool*)

(defun ~specialized-emacs? ()
  "Check if Emacs is running in specialized mode (mail browser,
note taker, ...)"
  (~emacs-as))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-config (&rest paths)
  "Returns path to a config file or directory."
  (apply 'concat *config-dir* paths))

(defun ~load-files (&rest paths)
  "Load Emacs Lisp source files when they exist."
  (dolist (file-path paths)
    (loop for possible-file-path in (list file-path
                                          (concat file-path ".el")
                                          (concat file-path ".elc"))
          when (and (file-exists-p possible-file-path)
                    (file-regular-p possible-file-path))
          do (progn (load file-path)
                    (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Byte compile the configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compile directory if not, and recompile if newer
;; (byte-recompile-directory *config-dir* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minimal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "
%s
Invoke debugger when error: %s
Emacs as: %s
"
         (emacs-version)
         debug-on-error
         (~emacs-as))

;; Load machine-specific-init settings if existed
(~load-files "~/.emacs-machine-specific-init"
             (~get-config "machine-specific-init"))

(require 'ee:config-package-manager      (~get-config "config-package-manager"))
(require 'ee:config-core-emacs-lisp      (~get-config "config-core-emacs-lisp"))
(require 'ee:config-core-functionalities (~get-config "config-core-functionalities"))
(require 'ee:config-edit                 (~get-config "config-edit"))
(require 'ee:functions-emacs-lisp        (~get-config "functions-emacs-lisp"))
(require 'ee:functions-buffer            (~get-config "functions-buffer"))
(require 'ee:functions-file              (~get-config "functions-file"))
(require 'ee:functions-process           (~get-config "functions-process"))
(require 'ee:functions-network           (~get-config "functions-network"))
(require 'ee:functions-string            (~get-config "functions-string"))
(require 'ee:functions-shell             (~get-config "functions-shell"))
(require 'ee:config-core-ux              (~get-config "config-core-ux"))
(require 'ee:config-ipc                  (~get-config "config-ipc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save minibuffer history across sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ref: https://www.emacswiki.org/emacs/SaveHist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(savehist-mode 1)
(setq savehist-file
      (format "~/.emacs.d/history.%s"
              (~emacs-server-name)))
(dolist (var '(kill-ring
               search-ring
               regexp-search-ring))
  (add-to-list 'savehist-additional-variables var))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ee:keybindings                 (~get-config "keybindings"))

(message "Finish loading basic functionalities")
