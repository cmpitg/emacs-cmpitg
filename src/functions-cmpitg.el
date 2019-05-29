;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2017-2019 Ha-Duong Nguyen (@cmpitg)
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

(defun ~jekyll-add-last-updated ()
  "Adds `last_update' timestamp with `date -R` format for
Jekyll."
  (interactive)
  (save-excursion
    (goto-point (point-min))
    (if (re-search-forward "^last_updated:.*$")
        (replace-match (format "last_updated: %s"
                               (string-trim (~exec "date -R")))))))

(defun* ~get-project-toolbox-path (&key (dir (~current-project-root))
                                        (file-name ".rmacs-toolbox"))
  "Gets the path to the current project toolbox file."
  (f-join dir file-name))

(defun* ~visit-project-toolbox ()
  "Visits the current project's toolbox file."
  (interactive)
  (find-file (~get-project-toolbox-path)))

(defun* ~toggle-project-toolbox (&key (side 'right)
                                      (size -78))
  "Toggles display of the project toolbox file."
  (interactive)
  (let* ((project-toolbox-path (~get-project-toolbox-path))
         (window (~toggle-toolbox :path project-toolbox-path
                                  :follow-dir nil)))
    window))

(defun* ~wmii/get-frame-id (&optional (frame (selected-frame)))
  "Gets the frame ID in Wmii-readable format."
  (thread-last (frame-parameter frame 'outer-window-id)
    string-to-number
    (format "0x%x")))

(defun* ~wmii/toggle-frame-floating (&optional (frame (selected-frame)))
  "Toggles the floating of a frame when using Wmii."
  (interactive)
  (~exec (format "wmiir xwrite /tag/sel/ctl send %s toggle"
                 (~wmii/get-frame-id frame))))

(defun* ~wmii/set-frame-floating (&optional (frame (selected-frame)))
  "Sets a frame to floating mode when using Wmii."
  (interactive)
  (~exec (format "wmiir xwrite /tag/sel/ctl send %s '~'"
                 (~wmii/get-frame-id frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg-specific functions")

(provide 'rmacs:functions-cmpitg)
