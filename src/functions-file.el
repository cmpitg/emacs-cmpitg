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

(defun ~deconstruct-path (path)
  "Deconstructs a path notation into `path' and `number' or
`pattern'.  See the following examples for further information:

\(~deconstruct-path \"/tmp/aoeu\"\)                       ⇒ \(values \"/tmp/aoeu\"\)
\(~deconstruct-path \"/tmp/aoeu:10\"\)                    ⇒ \(values \"/tmp/aoeu\" 10\)
\(~deconstruct-path \"/tmp/aoeu:10/other/:20\"\)          ⇒ \(values \"/tmp/aoeu:10/other/:20\" 20\)
\(~deconstruct-path \"/tmp/aoeu:/hello world/\"\)         ⇒ \(values \"/tmp/aoeu\" \"hello world\"\)
\(~deconstruct-path \"/tmp/aoeu:/inside:/hello world/\"\) ⇒ \(values \"/tmp/aoeu:/inside\" \"hello world\"\)
"
  (let ((matches (or (s-match (rx (group (one-or-more any))
                                  ":" (group (one-or-more digit))
                                  eol)
                              path)
                     (s-match (rx (group (one-or-more any))
                                  ":/" (group (one-or-more any)) "/" eol)
                              path))))
    (if matches
        (let* ((path (nth 1 matches))
               (pattern-or-number (nth 2 matches))
               (number (string-to-int pattern-or-number)))
          (if (zerop number)
              (values path pattern-or-number)
            (values path number)))
      (values path))))

(defun ~write-to-file (filename content)
  "Write string to file."
  (with-temp-buffer
    (insert content)
    (write-file filename)))

(defun ~list-dir (path)
  "List a directory content."
  (directory-files path))

(defun ~list-dir-full-path (path)
  "List a directory content with full path."
  (-map (lambda (file)
          (s-concat path "/" file)) (directory-files path)))

(defun ~read-file (path)
  "Read file and return file content as string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ~rename-current-file (&optional new-name)
  "Renames both current buffer and file it's visiting to
NEW-NAME."
  (interactive)
  (let* ((new-name (expand-file-name (cond ((not (~string-empty? new-name))
                                            new-name)
                                           (t
                                            (read-file-name "New name: ")))))
         (name (buffer-name))
         (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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

(defun ~current-dir ()
  "Current directory."
  (or (file-name-directory (or load-file-name buffer-file-name ""))
      "~"))

(defun ~current-file-full-path ()
  "Full path to current file."
  (or (expand-file-name buffer-file-name)
      ""))

(defun ~current-file-name ()
  "Current file name."
  (if buffer-file-name
    (concat (file-name-base buffer-file-name) "."
            (file-name-extension buffer-file-name))
    ""))

(defun ~current-file-name-without-extension ()
  "Return current file name without extension."
  (file-name-sans-extension (~current-file-name)))

(defun ~delete-current-file ()
  "Deletes the file associated with the current buffer and kills
off the buffer."
  (interactive)
  (let ((current-file (~current-file-full-path)))
    (when (yes-or-no-p (concat "Delete file: " current-file))
      ;; Prevent the following kill-buffer from recursively calling this function
      (when (local-variable-p 'local/delete-on-exit)
        (kill-local-variable 'local/delete-on-exit))
      (kill-buffer (current-buffer))
      (delete-file current-file)
      (message (concat "Deleted file: " current-file)))))
(defalias '~delete-this-file '~delete-current-file)

(defun ~open-current-file-as-admin ()
  "Open the current buffer as *nix root.
This command works on `sudo` *nixes only."
  (interactive)
  (when buffer-file-name
    (let* ((parsed-data (~parse-tramp-argument buffer-file-name))
           (username  (~alist-get parsed-data 'username))
           (host      (~alist-get parsed-data 'host))
           (path      (~alist-get parsed-data 'path))
           (port      (~alist-get parsed-data 'port)))
      (find-alternate-file
       (if (~string-empty? port)
           (format "/sudo:root@%s:%s"
                   host
                   path)
         ;; See Tramp's multiple hop
         (progn
           (message (format "/ssh:%s@%s#%s|sudo:%s#%s:%s"
                            username
                            host
                            port
                            host
                            port
                            path))
           (format "/ssh:%s@%s#%s|sudo:%s#%s/%s"
                   username
                   host
                   port
                   host
                   port
                   path)))))))

(defalias '~open-file 'find-file
  "Open a file")

(defalias '~open-file-other-window 'find-file-other-window
  "Open a file in other window")

(defalias '~file-exists? 'file-exists-p
  "Determine if a file exists")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading file-related functions")
(provide 'ee:functions-file)
