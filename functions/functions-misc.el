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
;;; Uncategorized functions

(defun start-emacs-server (&rest dir)
  "Start an Emacs server in a specific socket directory.  If no
directory is specified, the default dir /tmp/emacs1000/ is used.
Do nothing if server is already started."
  (if dir (setq server-socket-dir dir))
  (unless (file-exists-p server-socket-dir)
    (server-start)))
