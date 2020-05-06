;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2020 Ha-Duong Nguyen (@cmpitg)
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
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~activate-selection (pos-1 pos-2)
  "Activates a selection, also visually, then leaves the point at
`pos-2'."
  (interactive)
  (set-mark pos-1)
  (goto-char pos-2)
  (activate-mark))

(defun ~format-json ()
  "Formats current selection as JSON.  Requires jq."
  (interactive)
  (~exec| "jq ."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better UX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Revise
(defhydra hydra-read-input-async (:columns 4 :exit t)
  "Operations"
  ("RET" #'(lambda () (interactive) (funcall local/fn-accept-input)) "Accept")
  ("k" #'kill-current-buffer "Cancel"))

;; TODO: Revise and improve docstring
;; TODO: Support tooltip
;; TODO: History
(defun* ~read-multiple-inputs-async (&key prompts
                                          callback
                                          (title "Prompting")
                                          (size 70))
  "Displays a separate buffer to input multiple strings.  The
input is accepted with `C-c C-c' and discarded with `C-c C-k'.
When the input is accepted, The `callback' function, taking as
many values as the number of elements in the list of values as
its `PROMPTS', will be called.

`prompts' is an alist, each element has the format of `(<prompt>
. <default-value>)', denoting the prompt and its default value."
  (interactive)
  ;; Make sure the input window doesn't exist in any frame
  (when-let (wind (get-buffer-window title t))
    (delete-window wind))

  ;; Now create the window
  (let* ((accept-input #'(lambda ()
                           (interactive)
                           (goto-char 0)
                           (let ((values (loop
                                          for i from 1 to (length prompts)
                                          collect (progn (delete-field)
                                                         (let ((value (thread-first (field-string-no-properties)
                                                                        (seq-drop 1))))
                                                           (delete-field)
                                                           (delete-field)
                                                           value)))))
                             (kill-current-buffer)
                             (apply callback values))))
         (current-buffer (get-buffer-create title)))
    (with-current-buffer current-buffer
      ;; Display the content
      (~clean-up-buffer)
      (dolist (prompt&default-value prompts)
        (let ((prompt (car prompt&default-value))
              (default-value (cdr prompt&default-value)))
          (insert (propertize prompt
                              'field prompt)
                  (propertize (s-concat " " default-value)
                              'field (s-concat prompt "-value"))
                  (propertize "\n"
                              'field "newline"))))

      ;; Bind keys
      (setq-local local/fn-accept-input accept-input)

      ;; Display the buffer
      ;; (split-window (selected-window) size 'left)
      (switch-to-buffer current-buffer)
      (set-window-dedicated-p (selected-window) t))
    current-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window & Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-current-monitor-workarea (&optional display)
  "Returns the current position and size of the workarea for the
current monitor in the format of (X Y WIDTH HEIGHT)"
  (thread-last (display-monitor-attributes-list display)
    first
    (alist-get 'workarea)))

(defun ~centralize-mouse-position ()
  "Centralizes mouse position in the current window."
  (interactive)
  (unless (minibufferp (current-buffer))
    (let ((frame (selected-frame)))
      (destructuring-bind (x1 y1 x2 y2) (window-edges)
        (set-mouse-position frame
                            (+ x1 (/ (window-body-width) 2))
                            (+ y1 (/ (window-body-height) 2)))))))

(defun ~auto-pos-mouse-position ()
  "Automatically position mouse in a sensible way."
  (interactive)
  (when (eq (current-buffer) (window-buffer (selected-window)))
    (unless (minibufferp (current-buffer))
      (let ((frame (selected-frame)))
        (destructuring-bind (x1 y1 x2 y2) (window-edges)
          (set-mouse-position frame (+ x1 1) y1))))))

(defun* ~center-frame (width
                       height
                       &key
                       (frame (selected-frame))
                       (display nil))
  "Centers a frame with the width & height dimensions in
characters."
  (set-frame-size frame width height t)
  (destructuring-bind (_ _ screen-width screen-height) (~get-current-monitor-workarea display)
    (let* ((desired-x (/ (- screen-width width) 2))
           (desired-y (/ (- screen-height height) 2)))
      (set-frame-position frame desired-x desired-y))))

(defun* ~center-frame-in-chars (width-in-chars
                                height-in-chars
                                &key
                                (frame (selected-frame))
                                (display nil))
  "Centers a frame with the width & height dimensions in
characters."
  (set-frame-size frame width-in-chars height-in-chars)
  (let* ((width (frame-pixel-width frame))
         (height (frame-pixel-height frame)))
    (~center-frame width height
                   :frame frame
                   :display display)))

(provide 'rmacs:config-functions)
