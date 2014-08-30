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

(defun ~start-emacs-server (&rest dir)
  "Start an Emacs server in a specific socket directory.  If no
directory is specified, the default dir /tmp/emacs1000/server is
used.  Do nothing if server is already started."
  (setq server-socket-dir (if dir
                            dir
                            "/tmp/emacs1000"))
  (unless (and (boundp 'server-socket-dir)
               (file-exists-p (format "%s/server" server-socket-dir)))
    (server-start)))

(defalias '~get-local-config-path '~get-local-config-dir)
