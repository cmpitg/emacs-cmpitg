;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2017-2020 Ha-Duong Nguyen (@cmpitg)
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

(defun ~identity (x)
  "The identity function."
  x)

(defmacro ~comment (&rest body)
  "Ignores the body."
  nil)

(cl-defun ~get-project-toolbox-path (&key (dir (~get-current-project-root))
                                          (file-name ".rmacs-toolbox"))
  "Gets the path to the current project toolbox file."
  (f-join dir file-name))

(cl-defun ~get-project-sh-output-path (&key (dir (~get-current-project-root)))
  "Gets the path to the current project SH output file."
  (~get-project-toolbox-path :dir dir
                             :file-name ".rmacs-sh-output"))

(cl-defun ~open-project-toolbox ()
  "Opens the current project's toolbox file."
  (interactive)
  (find-file (~get-project-toolbox-path)))

(cl-defun ~open-project-sh-output ()
  "Opens the current project's SH output file."
  (interactive)
  (find-file (~get-project-sh-output-path)))

(defun ~open-project-notes ()
  "Opens current project notes."
  (interactive)
  (find-file (~get-project-toolbox-path :file-name ".notes")))

(defun ~format-opened-files ()
  "Formats all currently open files."
  (interactive)
  (string-join (loop for b in (buffer-list)
                     when (buffer-file-name b)
                     collect (buffer-file-name b))
               "\n"))

(cl-defun ~toggle-project-toolbox (&key (side 'right)
                                      (size -78))
  "Toggles display of the project toolbox file."
  (interactive)
  (let* ((project-toolbox-path (~get-project-toolbox-path))
         (window (~toggle-toolbox :path project-toolbox-path
                                  :follow-dir nil)))
    window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-purpose editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~insert-week-marker.fi ()
  "Inserts week marker, in FI locale"
  (interactive)
  (insert "Week ")
  (~insert-week-number))

(defun ~insert-week-number ()
  "Inserts the current week number with the current year"
  (interactive)
  (~exec-|-async ("date" "+%U/%Y")
                 #'string-trim
                 #'insert))

(defun ~insert-date.fi ()
  "Inserts the current date, in FI locale"
  (interactive)
  (~exec-|-async ("date" "+%d.%m.%Y")
                 #'string-trim
                 #'insert))

(defalias #'~insert-today #'~insert-date.fi)

(defun ~insert-date-and-time ()
  "Inserts the current date & time"
  (interactive)
  (~exec-|-async ("date" "-R")
                 #'string-trim
                 #'insert))

(defalias #'~insert-now #'~insert-date-and-time)

(defun ~insert-entry-with-timestamp ()
  "Inserts an entry with timestamp."
  (interactive)
  (call-interactively #'org-insert-heading)
  (call-interactively #'~insert-date-and-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window manager - WMII
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~wmii/get-frame-id (&optional (frame (selected-frame)))
  "Gets the frame ID in Wmii-readable format."
  (thread-last (frame-parameter frame 'outer-window-id)
    string-to-number
    (format "0x%x")))

(cl-defun ~wmii/toggle-frame-floating (&optional (frame (selected-frame)))
  "Toggles the floating of a frame when using Wmii."
  (interactive)
  (~exec-sh (list "wmiir" "xwrite" "/tag/sel/ctl" "send" (~wmii/get-frame-id frame) "toggle")))

(cl-defun ~wmii/set-frame-floating (&optional (frame (selected-frame)))
  "Sets a frame to floating mode when using Wmii."
  (interactive)
  (~exec-sh (list "wmiir" "xwrite" "/tag/sel/ctl" "send" (~wmii/get-frame-id frame) "~")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro* ~wm/call-awesome-client (args &optional (callback #'~identity))
  "Calls awesome-client (from Awesome WM)."
  `(~exec-|-async ("env" "AWESOME_RLWRAP=" "awesome-client" ,@args)
                  ,callback))

(defmacro* ~wm/call-herbstclient (args &optional (callback #'~identity))
  "Calls herbstclient (from Herbstluftwm)"
  `(~exec-|-async ("herbstclient" ,@args) ,callback))

(defun ~wm/get-focused-window-id ()
  "Gets the ID for the currently focused window."
  (string-trim (~exec-sh (list "xdotool" "getwindowfocus"))))

(defun ~wm/focus-window-by-id (window-id)
  "Focuses a window by its ID."
  (string-trim (~exec-sh (list "xdotool" "windowfocus" "--sync" window-id))))

(with-eval-after-load "hydra"
  (defhydra hydra-wm-app (:columns 4 :exit t)
    "Run application"
    ("a" #'(lambda () (interactive) (~dispatch-action "!@ config-inputs-cmpitg")) "Config input")
    ("m" #'(lambda () (interactive) (~dispatch-action "!@ konsole")) "Terminal emulator")
    ("w" #'(lambda () (interactive) (~dispatch-action "!@ web-browser-gui -ProfileManager")) "Web browser")
    ("l" #'(lambda () (interactive) (~dispatch-action "!@ lockscreen")) "Lock screen")
    ("s" #'(lambda () (interactive) (~dispatch-action "!@ suspend-me")) "Suspend")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading cmpitg-specific functions")

(provide 'rmacs:functions-cmpitg)
