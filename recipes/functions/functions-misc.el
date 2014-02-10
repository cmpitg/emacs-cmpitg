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
;;

(defun ~start-emacs-server (&rest dir)
  "Start an Emacs server in a specific socket directory.  If no
directory is specified, the default dir /tmp/emacs1000/server is
used.  Do nothing if server is already started."
  (setq server-socket-dir (if dir
                            dir
                            "/tmp/emacs1000/server"))
  (unless (and (~is-var-defined? 'server-socket-dir)
               (file-exists-p server-socket-dir))
    (server-start)))

(defun ~clipboard<-region (begin end)
  "Copy region to clipboard."
  (clipboard-kill-ring-save begin end))

(defun ~kill-ring<- (str)
  "Copy a string to the kill ring."
  (interactive "MString: ")
  (kill-new str))

(defun ~clipboard<- (str)
  "Copy a string to clipboard."
  (interactive "MString: ")
  (let ((x-select-enable-clipboard t))
    (x-select-text str)))

(defun ~clipboard<-pwd ()
  "Copy current directory to clipboard."
  (interactive)
  (~clipboard<- (~current-dir)))

(defun ~keyboard-quit ()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we
clicked away or set the cursor into another buffer) we can quit
by pressing 'ESC' three times. This function handles it more
conveniently, as it checks for the condition of not beign in the
minibuffer but having it active. Otherwise simply doing the ESC
or (keyboard-escape-quit) would brake whatever split of windows
we might have in the frame."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
    (if (or mark-active (active-minibuffer-window))
	  (keyboard-escape-quit))
    (keyboard-quit)))

(defun ~modify-opacity (&optional dec)
  "Modify the opacity of emacs frame; if DEC is t,
increase the opacity."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha))
         (old-alpha (if alpha-or-nil alpha-or-nil 100))
         (new-alpha (if dec (- old-alpha 10) (+ old-alpha 10))))
    (when (and (>= new-alpha frame-alpha-lower-limit) (<= new-alpha 100))
      (modify-frame-parameters nil (list (cons 'alpha new-alpha))))))

(defun ~put-mode-line-to-top ()
  "Put the mode-line to the top of the window."
  (setq header-line-format mode-line-format mode-line-format nil))

(defalias 'start-emacs-server '~start-emacs-server)
(defalias 'clipboard<-region '~clipboard<-region)
(defalias 'kill-ring<- '~kill-ring<-)
(defalias 'clipboard<- '~clipboard<-)
(defalias 'clipboard<-pwd '~clipboard<-pwd)
(defalias 'modify-opacity '~modify-opacity)
(defalias 'put-mode-line-to-top '~put-mode-line-to-top)
