;;
;; Copyright (C) 2017 Ha-Duong Nguyen (@cmpitg)
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

(defun ~create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -R %s"
           (executable-find "ctags")
           (directory-file-name dir-name))))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo
find-tag.  If buffer is modified, ask about save before running
etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them
silently."
  (interactive)
  (shell-command (format "%s -e -R *.%s"
                         *ctags-path*
                         (or extension "*")
                         ))
  (let ((tags-revert-without-query t)) ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(defun ~goto-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading tag processing functions")
(provide 'ee:functions-tag)
