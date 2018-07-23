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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables & parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *one-buffer-per-window-temporarily-disabled?* nil
  "Dynamic variable used to determine if the
one-buffer-per-window switching mechanism is temporarily
disabled.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* ~is-buffer-visible? (buffer-or-name)
  "Determines if a buffer is current visible."
  (not (null (get-buffer-window buffer-or-name 0))))

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

(defun* ~disable-one-buffer-per-window-for (funcs)
  "Disables one-buffer-per-window for certain functions."
  (dolist (func funcs)
    (advice-add func :around
                #'~advice/disable-one-buffer-per-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks & advices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Make sure switching to previous buffer falls back to the blank buffer if
;; the target buffer is already visible.
;;

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
;; Filter out the blank buffer when calling buffer list.
;;

(defun ~advice/filter-out-blank-buffer (orig-fun &rest args)
  "Filters out the blank buffer."
  (remove-if #'~is-blank-buffer? (apply orig-fun args)))

(defun ~advice/filter-out-blank-buffer-for-window-prev-buffers (orig-fun &rest args)
  "Filters out the blank buffer."
  (remove-if #'(lambda (x)
                 (~is-blank-buffer? (first x)))
             (apply orig-fun args)))

(advice-add 'buffer-list
            :around #'~advice/filter-out-blank-buffer)
(advice-add 'window-next-buffers
            :around #'~advice/filter-out-blank-buffer)
(advice-add 'window-prev-buffers
            :around #'~advice/filter-out-blank-buffer-for-window-prev-buffers)

;;
;; switch-to-buffer should focus the corresponding frame & window instead of
;; switching to the target buffer.
;;

(defun ~advice/change-focus-if-buffer-is-already-visible (orig-fun buffer-or-name &rest args)
  "Prevents switching to an already visible buffer unless it's
the blank buffer."
  (if (and (not *one-buffer-per-window-temporarily-disabled?*)
           (not (~is-blank-buffer? buffer-or-name))
           (~is-buffer-visible? buffer-or-name))
      (let* ((window (get-buffer-window buffer-or-name t))
             (frame (window-frame window)))
        (select-frame-set-input-focus frame)
        (select-window window))
    (apply orig-fun buffer-or-name args)))

(advice-add 'switch-to-buffer
            :around #'~advice/change-focus-if-buffer-is-already-visible)

;;
;; Sometimes we would like to temporarily disable one-buffer-per-window
;; behavior.
;;

(defun* ~advice/disable-one-buffer-per-window (orig-fun &rest args)
  "Advice to temporarily disable one-buffer-per-window."
  (let ((*one-buffer-per-window-temporarily-disabled?* t))
    (apply orig-fun args)))

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
;; Setting up & tearing down
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring one buffer per window")

(provide 'rmacs:config-one-buffer-per-window)
