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
(defun* ~read-input-string-async (&key prompt callback default-value (size 70))
  "Displays a separate buffer to read input string.  The input is
accepted with `C-c C-c' and discarded with `C-c C-k'.  When the
input is accepted, The `callback' function, taking the buffer
string as its only argument, will be called.

The buffered used for input uses `prompt' as it title and
`default-value' as its default value."
  (interactive)
  ;; Make sure the input window doesn't exist in any frame
  (when-let (wind (get-buffer-window prompt t))
    (delete-window wind))

  ;; Now create the window
  (let* ((accept-input #'(lambda ()
                           (interactive)
                           (let ((value (buffer-string)))
                             (kill-buffer-and-window)
                             (funcall callback value))))
         (current-buffer (get-buffer-create prompt)))
    (with-current-buffer current-buffer
      (~clean-up-buffer)
      (when default-value (insert default-value))
      (~bind-key-with-prefix "RET" accept-input :keymap (current-local-map))
      (bind-key "C-c C-c" accept-input (current-local-map))
      (bind-key "C-c C-k" #'kill-buffer-and-window (current-local-map))
      (split-window (selected-window) size 'left)
      (switch-to-buffer current-buffer)
      (set-window-dedicated-p (selected-window) t))
    current-buffer))

;; TODO: Revise
(defun* ~read-multiple-input-strings-async (&key prompts
                                                 callback
                                                 (title "Prompting")
                                                 (size 70))
  "Displays a separate buffer to input multiple strings.  The
input is accepted with `C-c C-c' and discarded with `C-c C-k'.
When the input is accepted, The `callback' function, taking the
buffer string as its only argument, will be called.

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
                             (kill-buffer-and-window)
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
      (~bind-key-with-prefix "RET" accept-input :keymap (current-local-map))
      (bind-key "C-c C-c" accept-input (current-local-map))
      (bind-key "C-c C-k" #'kill-buffer-and-window (current-local-map))

      ;; Display the buffer
      (split-window (selected-window) size 'left)
      (switch-to-buffer current-buffer)
      (set-window-dedicated-p (selected-window) t))
    current-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window & Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let* ((screen-width (display-pixel-width display))
         (screen-height (display-pixel-height display))
         (desired-x (/ (- screen-width width) 2))
         (desired-y (/ (- screen-height height) 2)))
    (set-frame-position frame desired-x desired-y)))

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
    (~center-frame width height frame display)))

(provide 'rmacs:config-functions)
