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

;; TODO: DOC me
(defun* ~is-buffer-visible? (buffer-or-name)
  "Determines if a buffer is current visible."
  (not (null (get-buffer-window buffer-or-name 0))))

;; TODO: DOC me
(defun* ~count-buffer-appearances (buffer-or-name)
  "Counts the number of windows where a buffer appearances."
  (length (get-buffer-window-list buffer-or-name nil 0)))

(defun ~get-blank-buffer ()
  "Gets the one and only blank, read-only buffer.  If the buffer
does not yet exist, create it."
  (let ((buffer (get-buffer-create "*Blank*")))
    (with-current-buffer buffer
      (read-only-mode 1))
    buffer))

(defun ~is-blank-buffer? (buffer-or-name)
  "Determines if a buffer is the blank buffer."
  (eq (get-buffer buffer-or-name) (~get-blank-buffer)))

(defun* ~switch-to-blank-buffer (&optional frame)
  "Switches to the blank buffer."
  (interactive)
  (when frame (select-frame frame))
  (switch-to-buffer (~get-blank-buffer)))

(defun ~switch-to-blank-buffer-other-window ()
  "Switches to the blank in the next window."
  (interactive)
  (call-interactively 'other-window)
  (call-interactively '~switch-to-blank-buffer))

(defun ~switch-buffer ()
  "Switches to another buffer that is not current visible.  This
function is based on the implementation of `ivy-switch-buffer'."
  (interactive)
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :predicate #'(lambda (buffer-cons)
                             (let ((buffer (cdr buffer-cons)))
                               (and (not (~is-buffer-visible? buffer))
                                    (not (~is-blank-buffer? buffer)))))
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks & advices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Make sure switching to previous buffer falls back to the blank buffer if
;; the target buffer is already visible.
;;

;; FIXME: Think more - If the current buffer is visible, keep trying until it's not
(defun ~advice/switch-to-blank-when-buffer-is-visible (orig-fun &rest args)
  "Switches to the blank buffer if the current buffer is already
visible."
  (let ((buffer (apply orig-fun args)))
    (if (and (not (null buffer))
             (not (~is-blank-buffer? buffer))
             (> (~count-buffer-appearances buffer) 1))
        (switch-to-buffer (~get-blank-buffer))
      buffer)))
(advice-add 'switch-to-prev-buffer
            :around #'~advice/switch-to-blank-when-buffer-is-visible)

;;
;; Filter out visible buffers when calling buffer list
;;

(defun ~advice/filter-out-visible-buffers (orig-fun &rest args)
  "Filters out visible buffers."
  (remove-if #'~is-buffer-visible? (apply orig-fun args)))

(defun ~advice/filter-out-visible-buffers-for-window-prev-buffers (orig-fun &rest args)
  "Filters out visible buffers."
  (remove-if #'(lambda (x)
                 (~is-buffer-visible? (first x)))
             (apply orig-fun args)))

(advice-add 'buffer-list
            :around #'~advice/filter-out-visible-buffers)
(advice-add 'window-next-buffers
            :around #'~advice/filter-out-visible-buffers)
(advice-add 'window-prev-buffers
            :around #'~advice/filter-out-visible-buffers-for-window-prev-buffers)

;;
;; Prevent switch-to-buffer from switching to a visible buffer except the
;; blank buffer.

(defun ~advice/prevent-switching-to-a-visible-buffer (orig-fun buffer-or-name &rest args)
  "Prevents switching to an already visible buffer unless it's
the blank buffer."
  (let ((buffer (get-buffer buffer-or-name)))
    (if (and (not (~is-blank-buffer? buffer))
             (~is-buffer-visible? buffer))
        (error "Cannot switch to %s as it is currently visible." buffer)
      (apply orig-fun buffer args))))

(advice-add 'switch-to-buffer
            :around #'~advice/prevent-switching-to-a-visible-buffer)

;;
;; Make sure after splitting window or creating a new frame, the blank buffer
;; is shown.
;;

(advice-add 'split-window-horizontally :after
            #'~switch-to-blank-buffer-other-window)
(advice-add 'split-window-vertically :after
            #'~switch-to-blank-buffer-other-window)
(add-hook 'after-make-frame-functions
          #'~switch-to-blank-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring one buffer per window")

(provide 'rmacs:config-one-buffer-per-window)
