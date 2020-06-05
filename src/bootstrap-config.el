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

;; TODO: Deprecated, use cl-lib instead 
(require 'cl)
(require 'cl-lib)

(if (string= "1" (getenv "EMACS_FORCE_TOGGLE_DEBUG_ON_ERROR"))
    (setq debug-on-error t)
  (setq debug-on-error nil))

(setq init-file-user (user-login-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure to spawn an Emacs frame when there is interactive prompt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (when (daemonp)
;;   (defun ~advice/ensure-frame (&rest _)
;;     "Ensures a frame on display :0.0 and ignore args."
;;     (let* ((display-list (x-display-list))
;;            (display-re (and display-list (regexp-opt display-list)))
;;            (term (and display-re (cl-some (lambda (term) (and (string-match display-re (terminal-name term)) term)) (terminal-list))))
;;            (frame (and term (cl-some (lambda (frame) (and (frame-live-p frame) frame)) (frames-on-display-list term)))))
;;       (unless frame
;;         (select-frame (make-frame-on-display (getenv "DISPLAY"))))
;;       ;; (select-frame (or frame (make-frame-on-display (getenv "DISPLAY"))))
;;       ))
;;   (advice-add 'y-or-n-p :before #'~advice/ensure-frame)
;;   (advice-add 'yes-or-no-p :before #'~advice/ensure-frame)
;;   (advice-add 'read-passwd :before #'~advice/ensure-frame))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-config (&rest paths)
  "Path to a config file or directory."
  (apply 'concat *config-dir* paths))

(defun ~load-files (&rest paths)
  "Loads Emacs Lisp source files when they exist."
  (dolist (file-path paths)
    (load file-path t nil)))

(defun ~reload-config ()
  "Reloads Rmacs config"
  (interactive)
  (~load-files (~get-config "init-" (substring (symbol-name *rmacs-shape*) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "%s :: debug-on-error=%s serser-name=%s init-file-user=%s\n"
         (emacs-version)
         debug-on-error
         server-name
         init-file-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On to configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path *config-dir*)

(~load-files "~/.emacs-machine-specific-init"
             (~get-config "machine-specific-init"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished bootstrapping configuration")

(provide 'rmacs:bootstrap-config)
