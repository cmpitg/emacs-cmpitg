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

(defun ~next-file-buffer ()
  "Move to the next non-special buffer, unless it's *scratch*."
  (interactive)
  (let* ((name "") (pos nil) (stop nil))
    (while (null stop)
      (setf name (buffer-name (next-buffer)))
      (setf pos (string-match "*" name))
      (if (string= "*scratch*" name) (setf stop t))
      (if (or (null pos)
              (> pos 0)) (setf stop t)))))

(defun ~move-to-compilation-buffer ()
  "Move to *compilation* buffer if it exists."
  (interactive)
  (if (find "*compilation*" (mapcar #'buffer-name (buffer-list))
            :test #'equal)
    (switch-to-buffer "*compilation*")))

(defun ~previous-buffer ()
  "Move to the previous non-special buffer, unless it's *scratch*."
  (interactive)
  (let* ((name "") (pos nil) (stop nil))
    (while (null stop)
      (setf name (buffer-name (previous-buffer)))
      (setf pos (string-match "*" name))
      (if (string= "*scratch*" name) (setf stop t))
      (if (or (null pos)
              (> pos 0)) (setf stop t)))))

(defun ~current-buffer-name ()
  "Retrieve the name of the current buffer."
  (buffer-name (current-buffer)))

(defun ~kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ~switch-to-last-buffer ()
  "Switch to last buffer."
  (interactive)
  (let ((old-name (~current-buffer-name)))
    (switch-to-buffer (other-buffer))
    (while (or (s-starts-with? "*" (~current-buffer-name))
               (s-equals? old-name (~current-buffer-name)))
      (previous-buffer))))

(defun ~last-sexp ()
  "Return the sexp right before the current cursor."
  (interactive)
  (preceding-sexp))

(defun ~geiser-repl-process ()
  "Return the process behind current Geiser REPL."
  (let ((repl-buffer (get-buffer "* Racket REPL *")))
    (if repl-buffer
      (get-buffer-process repl-buffer)
      nil)))

(defun ~geiser-send-string (string)
  "Evaluate last sexp with Geiser and send it to the REPL."
  (interactive)
  (let ((string-to-send (cond ((not (~string-empty? string))
                               string)
                              ((is-selecting?)
                               (get-selection))
                              (t
                               (~read-string "String: ")))))
    (comint-send-string (~geiser-repl-process) string)))

(defun ~current-line-comment-syntax ()
  "Return the current line-comment syntax for current buffer mode."
  comment-start)

(defun* ~popup-message (content &key (buffer-name "*Temporary*"))
  "Display a popup window with CONTENT as its content and an
optional BUFFER-NAME name.  Require popwin extension.  Press ESC
or C-g to close the window.

E.g.

;; Display \"Hello World\" in a popup window.
\($popup-message \"Hello World\"\)

;; Display \"Hola Mundo\" in a popup window, naming that window buffer \"*mundo*\"
\($popup-message \"Hello World\" :buffer-name \"*mundo*\"\)
"
  (with-output-to-temp-buffer buffer-name
    (princ content)))

(defun ~goto-str (str)
  "Go to the next appearance of a string."
  (interactive "MString: ")
  (search-forward str nil t))

(defun ~goto-snippets-dir ()
  "Go to personal snippets directory."
  (interactive)
  (find-file *snippet-dir*))

(defun ~goto-next-DEBUG ()
  "Go to next DEBUG."
  (interactive)
  (search-forward "DEBUG"))

(defun ~goto-prev-DEBUG ()
  "Go to prev DEBUG."
  (interactive)
  (search-backward "DEBUG"))

(defun ~goto-next-FIXME ()
  "Go to next FIXME."
  (interactive)
  (search-forward "FIXME"))

(defun ~goto-prev-FIXME ()
  "Go to prev FIXME."
  (interactive)
  (search-backward "FIXME"))

;; (defun ~switch-to-scratch ()
;;   "Switch to the *scratch* buffer."
;;   (interactive)
;;   (unless (string-equal "*scratch*" (~current-buffer-name))
;;     (switch-to-buffer-other-window "*scratch*")))

(defun ~switch-to-scratch ()
  "Switch to the scratch.el in `*scratch-dir*' directory."
  (interactive)
  (unless (string-equal "scratch.el" (~current-buffer-name))
    (unless (get-buffer "scratch.el")
      (find-file (s-concat *scratch-dir* "scratch.el")))
    (switch-to-buffer-other-window "scratch.el")))

(defun ~helm-multi-occur-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

(defun ~undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the
nth-killed buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
         (car recently-killed-list)))))

(defalias '~save-file 'save-buffer
  "Save current buffer")

(defalias '~popup-buffer 'popwin:popup-buffer)
(defalias 'popup-buffer '~popup-buffer)
(defalias 'next-file-buffer '~next-file-buffer)
(defalias 'move-to-compilation-buffer '~move-to-compilation-buffer)
(defalias 'current-buffer-name '~current-buffer-name)
(defalias 'kill-current-buffer '~kill-current-buffer)
(defalias 'switch-to-last-buffer '~switch-to-last-buffer)
(defalias 'last-sexp '~last-sexp)
(defalias 'geiser-repl-process '~geiser-repl-process)
(defalias 'geiser-send-string '~geiser-send-string)
(defalias 'popup-message '~popup-message)
(defalias 'goto-str '~goto-str)
(defalias 'goto-snippets-dir '~goto-snippets-dir)
(defalias 'goto-next-DEBUG '~goto-next-DEBUG)
(defalias 'goto-prev-DEBUG '~goto-prev-DEBUG)
(defalias 'goto-next-FIXME '~goto-next-FIXME)
(defalias 'goto-prev-FIXME '~goto-prev-FIXME)
(defalias 'undo-kill-buffer '~undo-kill-buffer)
