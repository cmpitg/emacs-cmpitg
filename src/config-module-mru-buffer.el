;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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

(use-package switch-buffer-functions
  :demand t
  :config
  (progn
    (defvar *mru-buffer-skip-list* (list (rx bol (0+ " ") "*")
                                         (rx bol "magit"))
      "List of regexps used to filter out buffers that shouldn't
      be in the MRU (most-recently-used) list.")

    (defvar *mru-buffer-list* (list)
      "List of MRU (most-recently-used) buffers in chronological order.")

    (defun mru-buffer:record-prev-buffer-name (prev-buffer current-buffer)
      "Records the `prev-buffer' to the MRU (most-recently-used)
list."
      (unless (or (minibufferp current-buffer)
                  (-any? #'(lambda (pattern)
                             (s-matches? pattern (buffer-name prev-buffer)))
                         *mru-buffer-skip-list*))
        (setq *mru-buffer-list* (remove* prev-buffer *mru-buffer-list*))
        (add-to-list '*mru-buffer-list* prev-buffer)))

    (defun mru-buffer:enable-mru ()
      "Enables the record of MRU (most-recently-used) buffer feature."
      (add-hook 'switch-buffer-functions #'mru-buffer:record-prev-buffer-name))

    (defun mru-buffer:disable-mru ()
      "Disables the record of MRU (most-recently-used) buffer feature."
      (remove-hook 'switch-buffer-functions #'mru-buffer:record-prev-buffer-name))

    (defun mru-buffer:switch-to-last-buffer ()
      "Switches to the MRU (most-recently-used) buffer."
      (interactive)
      (unless (null *mru-buffer-list*)
        (let ((dest-buffer (first *mru-buffer-list*)))
          (cond ((equalp dest-buffer (current-buffer))
                 (setq *mru-buffer-list* (rest *mru-buffer-list*))
                 (switch-to-buffer (first *mru-buffer-list*)))
                (t
                 (switch-to-buffer dest-buffer))))))

    (defun mru-buffer:reset-switch-buffer-functions ()
      "Resets the functionality of `switch-buffer-functions'
since it's easily broken."
      (interactive)
      (add-hook 'post-command-hook #'switch-buffer-functions-run)
      (remove-hook 'post-command-hook #'switch-buffer-functions-run))

    (mru-buffer:enable-mru)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring most-recently-used buffer feature")

(provide 'rmacs:config-module-mru-buffer)
