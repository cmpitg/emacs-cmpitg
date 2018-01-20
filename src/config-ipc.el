;;  -*- lexical-binding: t; -*-

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs-server-based IPC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server)

(eval-and-compile
  (defun* ~start-emacs-server (&optional (name server-name))
    "Starts the Emacs server."
    (message "Setting Emacs server name to %s and starting" name)
    (setf server-name name)
    (server-start))

  (defun ~emacs-server-name ()
    "Returns the current server name."
    server-name)

  (defun* ~emacs-server-running? (&optional (name server-name))
    "Determines if there is an Emacs server with `server-name' name
is currently running."
    (server-running-p name)))

(if (~emacs-server-running? (~emacs-server-name))
    (message "Emacs server %s is already running, not starting"
             (~emacs-server-name))
  (~start-emacs-server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading IPC capabilities")

(provide 'rmacs:config-ipc)
