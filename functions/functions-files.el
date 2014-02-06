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

(defun ~find-file-extended (&optional dir-path)
  "Find file with `fiplr' in a directory.  Symlinks are
followed."
  (interactive)
  (if (require 'fiplr nil nil)
    (let ((path (cond ((not (null dir-path))
                       dir-path)
                      ((~is-selecting?)
                       (~get-selection))
                      (t
                       (read-directory-name "Directory path: ")))))
      (fiplr-find-file-in-directory (file-chase-links path) fiplr-ignored-globs))
    (message "You need `fiplr' package to use this function.")))

(defun ~write-to-file (filename content)
  "Write string to file."
  (with-temp-buffer
    (insert content)
    (write-file filename)))

(defun ~make-executable ()
  "chmod +x current file."
  (interactive)
  (and
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (save-match-data
         (looking-at "^#!"))))
   (not (file-executable-p buffer-file-name))
   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
   (revert-buffer)
   (message
    (concat "Saved as script: " buffer-file-name))))

(defun ~list-dir (path)
  "List a directory content."
  (directory-files path))

(defun ~read-file (path)
  "Read file and return file content as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun* ~download-file (url filepath &key (overwrite nil))
  "Download a file.

E.g.

;; Download, raise an error if the file exists
\($download-file \"https://raw.github.com/defunkt/gist.el/master/gist.el\"
		\"/tmp/gist.el\"\)
c
;; Download and overwrite if already exists
\($download-file \"https://raw.github.com/defunkt/gist.el/master/gist.el\"
		\"/tmp/gist.el\"
		:overwrite t\)"
  (interactive "MURL: \nFSave to: ")
  (url-copy-file url filepath overwrite))

(defun ~scm-status ()
  "Call for the corresponding SCM `status` command."
  (interactive)
  (let ((current-scm (~get-scm)))
    (cond
     ((string= "git" current-scm)
      (magit-status nil))

     ((string= "hg" current-scm)
      (monky-status))

     (t nil))))

(defun ~get-scm ()
  "Return the current source control management (SCM) of current
file as string."
  (interactive)
  (let ((mode-name (downcase
                    (replace-regexp-in-string " \\|[[:digit:]]\\|:.*\\|-.*" ""
                                              (or vc-mode "")))))
    (cond ((and (~string-empty? mode-name)
                (magit-get-top-dir))
           "git")
          (t
           mode-name))))

(defun ~is-directory? (&optional text)
  "Determine if a portion of text is a directory on the
filesystem."
  (interactive)
  (let ((text (cond ((stringp text)
                     text)
                    ((~is-selecting? text)
                     (~current-selection))
                    (t
                     (read-string "Path: ")))))
    (f-dir? text)))

(defun ~open-file-gui ()
  "Open a file using Zenity."
  (interactive)
  (let ((filename (~string-but-last (~exec (~build-open-file-cmd-string)))))
    (unless (~string-empty? filename)
      (~open-file filename))))

(defun ~open-file-gui-other-window ()
  "Open a file using Zenity."
  (interactive)
  (let ((filename (~string-but-last (~exec (~build-open-file-cmd-string)))))
    (unless (~string-empty? filename)
      (~open-file-other-window filename))))

(defalias 'find-file-extended '~find-file-extended)
(defalias 'write-to-file '~write-to-file)
(defalias 'make-executable '~make-executable)
(defalias 'list-dir '~list-dir)
(defalias 'read-file '~read-file)
(defalias 'download-file '~download-file)
(defalias 'scm-status '~scm-status)
(defalias 'get-scm '~get-scm)
(defalias 'is-directory? '~is-directory?)
(defalias 'open-file-gui '~open-file-gui)
(defalias 'open-file-gui-other-window '~open-file-gui-other-window)
