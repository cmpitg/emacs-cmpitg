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

(defun ~eshell-history ()
  "Display eshell commands as with M-x.  The selected command is
added to the current eshell buffer."
  (interactive)
  (insert
   (ido-completing-read "Eshell history: "
                        (delete-dups
                         (ring-elements eshell-history-ring)))))

(defun ~switch-to-eshell-back-and-forth ()
  "Switch to eshell if current is not eshell, and switch to last
active buffer if current buffer is eshell."
  (interactive)
  (cond ((string-match-p "\\*.*eshell.*\\*" (~current-buffer-name))
         (~switch-to-last-buffer))
        (t
         (eshell))))

(defun ~execute-command-in-eshell (&optional command)
  "Execute a command in the only \*eshell\* buffer."
  (interactive)
  (let ((command (cond ((not (~string-empty? command))
                        command)
                       ((~is-selecting?)
                        (~current-selection))
                       (t
                        (read-string "Command: ")))))
    (with-current-buffer "*eshell*"
      (call-interactively 'end-of-buffer)
      (insert command)
      (call-interactively 'eshell-send-input))))

(defun ~execute-command-and-switch-to-eshell (&optional command)
  "Execute a command and then switch to \*eshell\* buffer."
  (interactive)
  (~execute-command-in-eshell command)
  (switch-to-buffer "*eshell*"))

(defun ~execute-command-and-popup-eshell (&optional command)
  "Execute a command and then popup the \*eshell\* buffer."
  (interactive)
  (~execute-command-in-eshell command)
  (with-current-buffer "*eshell*"
    (end-of-buffer))
  (~popup-buffer "*eshell*"))

(defun ~cd-and-switch-to-eshell (&optional path)
  "Change dir to `path' in eshell and jump to eshell buffer."
  (interactive)
  (let ((path (cond ((not (~string-empty? path))
                     path)
                    ((~is-selecting?)
                     (~current-selection))
                    (t
                     (read-directory-name "Path: ")))))
    (~execute-command-and-switch-to-eshell (s-concat "cd " path))))

(defun ~cd-current-buffer-dir-and-switch-to-eshell ()
  "Change dir to current dir of buffer and jump to eshell buffer.
If current buffer is not backed by file, switch to eshell."
  (interactive)
  (~cd-and-switch-to-eshell (~current-dir)))

(defalias 'eshell-history '~eshell-history)
(defalias 'switch-to-eshell-back-and-forth '~switch-to-eshell-back-and-forth)
(defalias 'execute-command-in-eshell '~execute-command-in-eshell)
(defalias 'execute-command-and-switch-to-eshell '~execute-command-and-switch-to-eshell)
(defalias 'cd-and-switch-to-eshell '~cd-and-switch-to-eshell)
(defalias 'cd-current-buffer-dir-and-switch-to-eshell '~cd-current-buffer-dir-and-switch-to-eshell)
(defalias 'execute-command-and-popup-eshell '~execute-command-and-popup-eshell)
