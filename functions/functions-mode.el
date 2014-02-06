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

(defun ~setup-moz-javascript ()
  "Setting JavaScript mode with MozRepl."
  (moz-minor-mode 1))

(defun ~sunrise ()
  "Open Sunrise Commander, remove the nonpane buffer."
  (interactive)
  (unless sr-running
    (sunrise)
    (sr-reset-view-remove-nonpane-buffer)))

(defun ~sunrise-cd ()
  "Open Sunrise Commander with current directory, remove the
nonpage buffer."
  (interactive)
  (unless sr-running
    (sunrise-cd)
    (sr-reset-view-remove-nonpane-buffer)))

(defun sr-reset-view ()
  "Reset Sunrise Commander pane view."
  (interactive)
  (when sr-running
    (sr-setup-windows)))

(defun sr-reset-view-remove-nonpane-buffer ()
  "Reset Sunrise Commander pane view, removing the nonpane
buffer."
  (interactive)
  (when sr-running
    (sr-setup-windows)
    (windmove-down)
    (delete-window)))

(defun ~auto-load-mode (filetypes mode)
  "Autoload mode for filetype regex or a list of filetypes.

Example:

    \($auto-load-mode \"\\\\.rake$\" 'ruby-mode\)
    \($auto-load-mode '(\"\\\\.md$\" \"\\\\.markdown$\") 'markdown-mode\)"
  (if (stringp filetypes)
    (add-to-list 'auto-mode-alist (cons filetypes mode))
    (dolist (filetype filetypes)
      (add-to-list 'auto-mode-alist (cons filetype mode)))))

(defalias 'setup-moz-javascript '~setup-moz-javascript)
(defalias 'auto-load-mode '~auto-load-mode)
