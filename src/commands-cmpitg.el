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

(defun ~start-ip-webcam ()
  "Starts streaming video from an IP webcam to a loopback device
so that the system could use it as a webcam."
  (interactive)
  (~read-multiple-inputs-async
   :prompts `(("Webcam device path:" . "/dev/video0")
              ("Source protocol:" . "http")
              ("Source IP:" . "192.168.1.106")
              ("Source port:" . "8080")
              ("Source path:" . "/videofeed")
              ("Width:" . "1280")
              ("Height:" . "720"))
   :callback #'(lambda (device-path protocol ip port path width height)
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
                                               "!" (format "video/x-raw,format=YUY2,width=%s,height=%s" width height)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'rmacs:commands-cmpitg)
