;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2020 Ha-Duong Nguyen (@cmpitg)
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
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~create-commander-frame (name &key wm frame-params)
  "Creates a special frame that is use to execute non-Emacs commands.  TODO"
  (let* ((f (make-frame `(,@frame-params
                          (name . ,name)
                          (user-position . t)
                          (user-size . t)
                          (auto-raise . t)
                          (skip-taskbar . t)
                          (alpha . 90)
                          (sticky . t)
                          (fullscreen . fullboth)
                          (undecorated . t))))
         (win-id (frame-parameter f 'parent-id)))
    (select-frame-set-input-focus f)
    (when (null (frame-parameter f 'fullscreen))
      (cl-case wm
        (:awesome
         (~wm/call-awesome-client
          ("client.focus.floating = true; client.focus.ontop = true")
          #'(lambda (_output)
              (~center-frame-percent 80 80 :frame f))))
        (:herbstluft
         t)
        (otherwise
         (~center-frame-percent 80 80 :frame f))))
    f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~start-ip-webcam ()
  "Starts streaming video from an IP webcam to a loopback device
so that the system could use it as a webcam."
  (interactive)
  (~read-multiple-inputs-async
   :prompts `(("Webcam device path:" . "/dev/video0")
              ("Source protocol:" . "http")
              ("Source IP:" . "192.168.1.83")
              ("Source port:" . "8080")
              ("Source path:" . "/videofeed")
              ("Frame rate:" . "24/1")
              ("Width:" . "1280")
              ("Height:" . "720"))
   :callback #'(lambda (device-path protocol ip port path frame-rate width height)
                 (let ((cmd (string-join (list "gst-launch-1.0" "-e" "-vt" "--gst-plugin-spew"
                                               "souphttpsrc" (format "location=%s://%s:%s%s"
                                                                     (downcase protocol)
                                                                     ip
                                                                     port
                                                                     path)
                                               "do-timestamp=true" "is-live=true"
                                               "!" "queue"
                                               "!" "multipartdemux"
                                               "!" "decodebin"
                                               "!" "videoconvert"
                                               "!" "videoscale"
                                               "!" "videorate"
                                               "!" (format "video/x-raw,format=YUY2,width=%s,height=%s,framerate=%s" width height frame-rate)
                                               "!" "v4l2sink" (format "device=%s" device-path) "sync=false")
                                         " ")))
                   (~dispatch-action (format "!! %s" cmd))))))

(defun ~start-xephyr-for-testing ()
  "Starts a Xephyr for WM testing"
  (interactive)
  (~read-multiple-inputs-async
   :prompts `(("DISPLAY" . ":11")
              ("Resolution" . "800x600"))
   :callback #'(lambda (display res)
                 (let ((cmd (string-join (list "Xephyr" "-br" "-ac" "-noreset" "-screen" res display)
                                         " ")))
                   (~dispatch-action (format "!! %s" cmd))))))

(defun ~capture-to-local-setup (&optional source dest)
  "Captures a file/directory to local setup."
  (interactive)
  (let* ((source (read-file-name "Source: "))
         (dest (read-file-name "Destination: " (f-join (getenv "MY_LOCAL_SETUP")
                                                       "src" "code")
                               nil
                               nil))
         (dest-dir (f-dirname dest))
         _ (when (and (f-exists? dest)
                      (y-or-n-p (format "'%s' exists, continue? " dest))))
         (visit-dest? (y-or-n-p "Visit destination? "))
         (delete-source? (y-or-n-p "Delete source? ")))
    (f-mkdir dest-dir)
    (f-copy source dest)
    (when visit-dest?
      (find-file dest))
    (when delete-source?
      (delete-file source))
    (message "'%s' captured to '%s'" source dest)))

(defun ~capture-to-local-data (&optional source dest)
  "Captures a file/directory to local data and symlinks back."
  (interactive)
  (let* ((source (read-file-name "Source: "))
         (dest (read-file-name "Destination: " (getenv "MY_DATA")
                               nil nil nil))
         (dest-dir (f-dirname dest)))
    (when (f-exists? dest)
      (error "'%s' must not exist yet" dest))
    (f-mkdir dest-dir)
    (f-move source dest)
    (f-symlink dest source)
    (message "'%s' captured to '%s'" source dest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hephaestus integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *~hephaestus-path* "/m/src/hephaestus/hephaestus"
  "Path to Hephaestus binary.")

(defun ~hephaestus/exec-this-file ()
  "Execs current file as recipe with Hephaestus."
  (interactive)
  (lexical-let ((path (~current-file-full-path)))
    (when (null path)
      (error "Current buffer doesn't have a file backed!"))
    (~palette/exec-sh-in-term-mux-then-pause (format "%s exec-recipe-file %s"
                                                     (shell-quote-argument *~hephaestus-path*)
                                                     (shell-quote-argument path)))))

(defun ~hephaestus/validate-this-file ()
  "Validates current recipe file with Hephaestus."
  (interactive)
  (lexical-let ((path (~current-file-full-path)))
    (when (null path)
      (error "Current buffer doesn't have a file backed!"))
    (~palette/exec-sh-in-term-mux-then-pause (format "%s validate-recipe-file %s"
                                                     (shell-quote-argument *~hephaestus-path*)
                                                     (shell-quote-argument path)))))

(defun ~hephaestus/exec-recipe-region ()
  "Execs current region as Hephaestus recipe."
  (interactive)
  (lexical-let* ((path (make-temp-file "rmacs-hephaestus_"))
                 (content (~get-selection)))
    (~write-to-file path content)
    (~palette/exec-sh-in-term-mux-then-pause (format "%s exec-recipe-file %s"
                                                     (shell-quote-argument *~hephaestus-path*)
                                                     (shell-quote-argument path)))))

(defun ~hephaestus/exec-recipe-current-block ()
  "Execs current block as Hephaestus recipe."
  (interactive)
  (save-mark-and-excursion
    (call-interactively #'~mark-current-block)
    (call-interactively #'~hephaestus/exec-recipe-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rmacs:commands-cmpitg)
