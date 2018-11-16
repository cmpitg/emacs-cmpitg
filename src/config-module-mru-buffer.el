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
      "List of MRU (most-recently-used) buffers in chronological
      order.")

    (defun mru-buffer:buffer-satisfies-skip-list? (buffer)
      "Checks if a buffer satisfies the skip list (defined be the
      `*mru-buffer-skip-list*' variable."
      (let ((buffer (get-buffer buffer)))
        (and (buffer-live-p buffer)
             (not (minibufferp buffer))
             (not (-any? #'(lambda (pattern)
                             (s-matches? pattern (buffer-name buffer)))
                         *mru-buffer-skip-list*)))))

    (defun mru-buffer:check-and-record-buffer (buffer)
      "Checks and records `buffer' to the `mru-buffer-list' if
      satisfies."
      (let ((buffer (get-buffer buffer)))
        ;; (message "checking %S" buffer)
        (when (mru-buffer:buffer-satisfies-skip-list? buffer)
          (setq *mru-buffer-list* (remove* buffer *mru-buffer-list*))
          (add-to-list '*mru-buffer-list* buffer))))

    (defun mru-buffer:advice/record-current-buffer (orig-fun buffer-or-name &rest args)
      "Records current buffer to the `mru-buffer-list' list."
      (condition-case ex
          (mru-buffer:check-and-record-buffer buffer-or-name)
        ('error (message "Error when recording buffer %S to the MRU list: %S"
                         buffer-or-name
                         ex)))
      (apply orig-fun buffer-or-name args))

    (defun mru-buffer:record-buffer-with-prev (prev-buffer current-buffer)
      "Records the current buffer to the MRU (most-recently-used)
      list."
      (condition-case ex
          (mru-buffer:check-and-record-buffer current-buffer)
        ('error (message "Error when recording buffer: %S to thu MRU list: %S. Previous buffer: %S"
                         current-buffer
                         ex
                         prev-buffer))))

    (defun mru-buffer:record-buffer-in-find-file ()
      "Records the current buffer after find-file.  This function
      is meant to be added to `find-file-hook'."
      (interactive)
      (condition-case ex
          (mru-buffer:check-and-record-buffer (current-buffer))
        ('error (message "Error when recording current buffer in find-file: %S"
                         ex))))

    (defun mru-buffer:enable-mru ()
      "Enables the record of MRU (most-recently-used) buffer feature."
      (interactive)
      (add-hook 'switch-buffer-functions #'mru-buffer:record-buffer-with-prev)
      (add-hook 'find-file-hook #'mru-buffer:record-buffer-in-find-file)
      (advice-add 'switch-to-buffer
                  :around #'mru-buffer:advice/record-current-buffer))

    (defun mru-buffer:disable-mru ()
      "Disables the record of MRU (most-recently-used) buffer feature."
      (interactive)
      (remove-hook 'switch-buffer-functions #'mru-buffer:record-buffer-with-buffer)
      (remove-hook 'find-file-hook #'mru-buffer:record-buffer-in-find-file)
      (advice-remove 'switch-to-buffer #'mru-buffer:advice/record-current-buffer))

    (defun mru-buffer:switch-to-last-buffer ()
      "Switches to the MRU (most-recently-used) buffer."
      (interactive)
      (condition-case ex
          (progn
            (setq *mru-buffer-list*
                  (remove-if-not #'buffer-live-p *mru-buffer-list*))
            (if (= 1 (length *mru-buffer-list*))
                (switch-to-buffer (first *mru-buffer-list*))
              (when (> (length *mru-buffer-list*) 1)
                (if (equalp (first *mru-buffer-list*) (current-buffer))
                    (switch-to-buffer (second *mru-buffer-list*))
                  (switch-to-buffer (first *mru-buffer-list*))))))
        ('error (message "Error when switching to last buffer: %S. Buffer list: %S"
                         ex
                         *mru-buffer-list*))))

    (defun mru-buffer:reset-switch-buffer-functions ()
      "Resets the functionality of `switch-buffer-functions'
since it's easily broken."
      (interactive)
      (remove-hook 'post-command-hook #'switch-buffer-functions-run)
      (add-hook 'post-command-hook #'switch-buffer-functions-run))

    (mru-buffer:enable-mru)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring most-recently-used buffer feature")

(provide 'rmacs:config-module-mru-buffer)
