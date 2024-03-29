;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2019-2021 Ha-Duong Nguyen (@cmpitg)
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

;;
;; Notes
;;
;; - Command palette is about having a special buffer-backed window that
;;   provides commands
;;
;; - Each visible window belongs to a visible frame
;;
;; - Command palette buffer:
;;   * links to the main buffer via local/main-buffer
;;   * links to its file via local/cp-path
;;   * has the default directory of the project directory of main buffer (if possible)
;;   * executes commands in the main buffer, unless specified otherwise
;;   * might and should belong to a command palette window
;;   * cannot be killed if the main buffer is visible -> TODO: helper to check if a buffer is visible
;;
;; - Command palette window:
;;   * must contain a blank buffer or a command palette buffer
;;   * links to the main window via local/main-window
;;   * is deleted when the corresponding main window (in the same frame) is deleted
;;   * is automatically created when a non-exceptional buffer is switched to
;;   * is automatically made fit when created
;;
;; - Each non-exceptional window has a command palette window
;;
;; - Windows are frame-dependent so one command palette buffer might have
;;   multiple main windows.  However, all of the main windows should belong to
;;   the same main buffer.
;;
;; - Killing the command palette buffer kills the main buffer and vice versa
;;
;; - Command palette is per project
;;

;; TODO: Decouple the execute-text function here from the Wand package

;; TODO: Default content template
;; TODO: Mode-specific content
;; TODO: Refactor using buffer-local-value
;; TODO: Refactor: get-buffer-window -> selected-window
;; TODO: See how the performance of the loop is, then refactor with links from the main buffer
;; TODO: Implement ~get-current-project-root here
;; TODO: Implement ~count-non-sticky-windows here

(require 'wand)

(defvar command-palette:*cp-window-face*
  '(:family "Go" :height 110))

(defvar command-palette:*exec-fn*
  #'wand:execute)

(defvar command-palette:*default-content*
  ""
  "Default content for the command palette buffer, which is
  displayed from the second line onward.")

(defvar command-palette:*conditional-content*
  (list (cons #'(lambda () (eq 'clojure-mode major-mode))
              "~cider-connect clojure-align")))

(defvar command-palette:*buffer-exception-regexp-list*
  (list
   ;; (rx bol "*" (0+ any) "*" eol)
   (rx bol "*cp:" (0+ any) "*" eol)
   (rx bol "*Blank*" eol)
   (rx bol "magit" (0+ any) eol))
  "")

;; TODO - Use the following to refactor other parts
;; (dolist (local-var-name '(local/buffer-features
;;                           local/main-window
;;                           local/main-buffer
;;                           local/cp-buffer
;;                           local/cp-path))
;;   (add-to-list 'safe-local-variable-values `(,local-var-name . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun command-palette:construct-content (main-buffer main-path)
  "Constructs the content for a command palette buffer."
  (with-current-buffer main-buffer
    (let ((conditional-content (thread-last
                                   (loop for x in command-palette:*conditional-content*
                                         when (destructuring-bind (condition . content) x
                                                (funcall condition))
                                         collect (cdr x))
                                 (s-join " "))))
      (if (string-empty-p conditional-content)
          (format "%s\n%s" main-path command-palette:*default-content*)
        (format "%s\n%s %s" main-path command-palette:*default-content* conditional-content)))))

(cl-defun command-palette:is-buffer-exception? (buffer-or-name)
  "Determines if the buffer is not supposed to have a command-palette window."
  (-any? (lambda (regexp)
           (~string-matches? regexp (buffer-name (get-buffer buffer-or-name))))
         command-palette:*buffer-exception-regexp-list*))

(cl-defun command-palette:is-command-palette-window? (&optional (window (selected-window)))
  "Determines if `window' is a command palette window."
  (and (window-dedicated-p window)
       (-contains? (window-parameter window :local/window-features) :command-palette)
       (not (null (window-parameter window :local/main-window)))))

(cl-defun command-palette:command-palette-window-exists? (&optional (window (selected-window)))
  "Determines if a command palette window exists for a specific
window."
  (when-let (cp-window (~get-non-minibuffer-window-in-dir 'up))
    (command-palette:is-command-palette-window? cp-window)))

(cl-defun command-palette:get-main-window (&optional (cp-window (selected-window)))
  "Gets the main window for a command palette window."
  (window-parameter cp-window :local/main-window))

(cl-defun command-palette:setup-command-palette-window (main-window cp-window)
  "Sets up necessary parameters for a pair of main & command
palette windows."
  (set-window-parameter cp-window :local/window-features '(:command-palette))

  ;; Link the windows together
  (set-window-parameter cp-window :local/main-window main-window)
  (set-window-parameter main-window :local/cp-window cp-window)

  ;; Finally, set special properties for the command palette window
  (with-selected-window cp-window
   (buffer-face-set command-palette:*cp-window-face*))
  (set-window-dedicated-p cp-window t))

(cl-defun command-palette:get-command-palette-window (&optional (window (selected-window)))
  "Gets the corresponding command palette window.  The command
palette window if exists is always the immediate window above.
If the current window doesn't have a command palette window,
returns `nil'."
  (when (command-palette:command-palette-window-exists? window)
    (~get-non-minibuffer-window-in-dir 'up)))

(cl-defun command-palette:fit-command-palette-window (&optional (cp-window (selected-window)))
  "Refits the command palette window."
  (interactive)
  (with-selected-window cp-window
    (fit-window-to-buffer)
    (recenter-top-bottom -1)))

(defun command-palette:switch-to-command-palette-buffer (cp-buffer)
  "Switches to the command palette buffer."
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer cp-buffer)
  (with-current-buffer cp-buffer
    (setq mode-line-format nil))
  (buffer-face-set command-palette:*cp-window-face*)
  (command-palette:fit-command-palette-window)
  (set-window-dedicated-p (selected-window) t)
  cp-buffer)

(cl-defun command-palette:ensure-command-palette-buffer (&optional (main-buffer (current-buffer)))
  "Ensures a command palette buffer exists for a main buffer."
  (interactive)
  (unless (command-palette:is-buffer-exception? main-buffer)
    (let* ((main-path (if (buffer-file-name main-buffer)
                          (buffer-file-name main-buffer)
                        (buffer-name)))
           (main-dir (with-current-buffer main-buffer (~get-current-project-root)))
           (cp-path (f-join main-dir ".rmacs-cp"))
           (cp-buffer-name (format "*cp:%s*" main-dir))
           (cp-buffer (get-buffer-create cp-buffer-name))
           (this-command.saved this-command))
      (with-current-buffer cp-buffer
        (if (string-empty-p (string-trim (buffer-string)))
            (progn
              (erase-buffer)
              (unless (eq 'emacs-lisp-mode major-mode)
                (emacs-lisp-mode))
              ;; (evil-mode 1)
              (if (f-exists? cp-path)
                  (insert-file-contents cp-path)
                (insert (command-palette:construct-content main-buffer main-path))))
          (progn
            (goto-char (point-min))
            (delete-region (point) (point-at-eol))
            (goto-char (point-min))
            (insert main-path)))

        (defvar-local local/buffer-features nil)
        (add-to-list 'local/buffer-features :command-palette)

        ;; Link buffers together
        (setq-local local/main-buffer main-buffer)
        (with-current-buffer main-buffer
          (setq-local local/cp-buffer cp-buffer))

        (setq-local local/main-path main-path)
        (setq-local local/cp-path cp-path)
        (setq-local default-directory (~get-current-project-root))

        ;; We don't want to record possible buffer manipulations as part of
        ;; the current command
        (setq this-command this-command.saved))
      cp-buffer)))

(cl-defun command-palette:save-cp (&optional (main-buffer (current-buffer)))
  "Saves the current command palette buffer."
  (interactive)
  (save-excursion
    (let ((cp-buffer (cond ((boundp 'local/main-buffer)
                            (current-buffer))
                           ((boundp 'local/cp-buffer)
                            local/cp-buffer)
                           (t
                            (error "Main buffer=%s doesn't have a command palette buffer" main-buffer)))))
      (with-current-buffer cp-buffer
        (~write-to-file local/cp-path (buffer-string))))))

(cl-defun command-palette:ensure-command-palette-window (&optional (main-window (selected-window)))
  "Ensures that a command palette window exists for a main window."
  (interactive)
  (require 'windmove)
  (cond ((command-palette:command-palette-window-exists? main-window)
         (let ((cp-window (command-palette:get-command-palette-window main-window)))
           (command-palette:setup-command-palette-window main-window cp-window)
           cp-window))
        (t
         (save-excursion
           ;; The command palette window is always right above the main window
           (split-window main-window nil 'above)
           (windmove-up)
           ;; (~switch-to-blank-buffer)
           (command-palette:setup-command-palette-window main-window (selected-window))
           (command-palette:fit-command-palette-window (selected-window))
           (selected-window)))))

(cl-defun command-palette:ensure-command-palette (&optional (main-window (selected-window)))
  "Ensures that a command palette window and buffer for the
current main window is properly set up.  If the current main
window contains an exceptional buffer, delete the existing
command palette window if exists."
  (interactive)
  (unless (command-palette:is-command-palette-window? main-window)
    (let ((main-buffer (window-buffer main-window)))
      (if-let (cp-buffer (command-palette:ensure-command-palette-buffer main-buffer))
          (let ((cp-window (command-palette:ensure-command-palette-window main-window)))
            (with-selected-window cp-window
              ;; Create the command palette buffer and switch to that buffer
              (command-palette:switch-to-command-palette-buffer cp-buffer)))
        ;; Most of the time we would want to work on the main buffer immediately
        ;; so let's move to that window
        (select-window main-window)

        ;; TODO: Errorneous
        ;; (let ((presumed-cp-window (ignore-errors (save-excursion (windmove-up)))))
        ;;   (when (and (not (null presumed-cp-window))
        ;;              (command-palette:is-command-palette-window? presumed-cp-window)
        ;;              (window-live-p presumed-cp-window))
        ;;     (delete-window presumed-cp-window)))
        ))))

(defun command-palette:try-fitting-cp-window ()
  "Tries fitting the command palette window if necessary and
possible."
  (interactive)
  (when (command-palette:is-command-palette-window?)
    (command-palette:fit-command-palette-window))
  (when-let (cp-window (command-palette:get-command-palette-window))
    (command-palette:fit-command-palette-window cp-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun command-palette:advice/ensure-command-palette (orig-fun &rest args)
  "Advice to ensure command palette exists and properly setup for
non-exceptional buffers."
  (let* ((res (apply orig-fun args))
         (window (selected-window)))
    (command-palette:ensure-command-palette)
    (select-window window)
    res))

(defun command-palette:advice/split-command-palette-window (split-fn &rest args)
  "TODO"
  (require 'windmove)
  (let* ((window (or (first args) (selected-window)))
         (size (second args))
         (side (if (eq t (third args))
                   'right
                 (third args)))
         (fake-cp-window
          (when (and (not (command-palette:is-command-palette-window? window))
                     (command-palette:command-palette-window-exists? window)
                     (-contains? '(left right) side))
            (save-excursion
              (with-selected-window (command-palette:get-command-palette-window window)
                ;; Split the current command palette window to create a fake command
                ;; palette
                (apply #'split-window (selected-window) size side (-drop 3 args))
                (save-excursion
                  (if (eq 'left side) (windmove-left) (windmove-right))
                  (~switch-to-blank-buffer)

                  ;; Make the splitted window a fake command palette window
                  (set-window-dedicated-p (selected-window) nil)
                  (set-window-parameter (selected-window) :local/window-features '(:command-palette))
                  (set-window-parameter (selected-window) :local/main-window window)
                  (set-window-dedicated-p (selected-window) t)
                  (selected-window))))))
         (res (apply split-fn args)))
    ;; Make sure the fake command palette window gets deleted when the main
    ;; window is deleted
    (unless (or (command-palette:is-command-palette-window? res)
                (null fake-cp-window))
      (set-window-parameter res :local/cp-window fake-cp-window))
    res))

(defun command-palette:advice/maybe-delete-window (orig-fun window &rest args)
  "Deletes the non-sticky window only if the number of non-sticky
windows is greater than 1."
  (if (and (or (window-dedicated-p window)
               (> (~count-non-sticky-windows) 1))
           (window-live-p window))
      (apply orig-fun window args)
    (message "Cannot delete the only non-sticky window=%s" window)))

(defun command-palette:advice/delete-command-palette-window (orig-fun window &rest args)
  "Deletes the corresponding command palette windows."
  (let* ((cp-window (window-parameter window :local/cp-window))
         (res (apply orig-fun window args)))
    (when (and (not (null cp-window))
               (window-live-p cp-window))
      (delete-window cp-window))
    res))

(cl-defun command-palette:advice/fit-command-palette-window (orig-fun &rest args)
  "Fits the command palette window."
  (command-palette:try-fitting-cp-window)
  (apply orig-fun args))

(cl-defun command-palette:enable ()
  "TODO"
  (interactive)
  (add-variable-watcher 'default-directory #'command-palette:default-dir-watcher)
  (advice-add 'top-level :around #'command-palette:advice/ensure-command-palette)
  (advice-add 'switch-to-buffer :around #'command-palette:advice/ensure-command-palette)
  (advice-add 'pop-to-buffer :around #'command-palette:advice/ensure-command-palette)
  (advice-add 'switch-to-prev-buffer :around #'command-palette:advice/ensure-command-palette)
  (advice-add 'find-file :around #'command-palette:advice/ensure-command-palette)
  (advice-add 'magit-status :around #'command-palette:advice/ensure-command-palette)
  (advice-add 'delete-window :around #'command-palette:advice/maybe-delete-window)
  (advice-add 'delete-window :around #'command-palette:advice/delete-command-palette-window)
  (advice-add 'split-window :around #'command-palette:advice/split-command-palette-window)
  (advice-add 'other-window :around #'command-palette:advice/fit-command-palette-window)
  (add-hook 'mouse-leave-buffer-hook #'command-palette:try-fitting-cp-window))

(cl-defun command-palette:disable ()
  "TODO"
  (interactive)
  (remove-variable-watcher 'default-directory #'command-palette:default-dir-watcher)
  (advice-remove 'top-level #'command-palette:advice/ensure-command-palette)
  (advice-remove 'switch-to-buffer #'command-palette:advice/ensure-command-palette)
  (advice-remove 'pop-to-buffer #'command-palette:advice/ensure-command-palette)
  (advice-remove 'switch-to-prev-buffer #'command-palette:advice/ensure-command-palette)
  (advice-remove 'find-file #'command-palette:advice/ensure-command-palette)
  (advice-remove 'magit-status #'command-palette:advice/ensure-command-palette)
  (advice-remove 'delete-window #'command-palette:advice/maybe-delete-window)
  (advice-remove 'delete-window #'command-palette:advice/delete-command-palette-window)
  (advice-remove 'split-window #'command-palette:advice/split-command-palette-window)
  (advice-remove 'other-window #'command-palette:advice/fit-command-palette-window)
  (remove-hook 'mouse-leave-buffer-hook #'command-palette:try-fitting-cp-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: This is a hack.  I haven't yet found a better way.
(let ((catching?-hash (make-hash-table :test #'equal))
      (new-dirs-hash (make-hash-table :test #'equal)))
  (cl-defun command-palette:default-dir-watcher (symbol new-val operation buffer)
    (when (and (eq 'default-directory symbol)
               (eq 'set operation)
               (gethash buffer catching?-hash))
      (message "default-directory is modified in buffer=%s" buffer)
      (puthash buffer new-val new-dirs-hash)))

  (cl-defun command-palette:call-with-current-dir (dir fn)
    "Calls `fn' with `dir' being the current working directory.
This function makes sure that the `default-directory' is not
shadowed when the call ends."
    (lexical-let ((result nil))
      (puthash (current-buffer) nil new-dirs-hash)
      (puthash (current-buffer) t catching?-hash)
      (condition-case err
          (let* ((default-directory dir))
            (setq result (funcall fn)))
        (error
         (puthash (current-buffer) nil new-dirs-hash)
         (puthash (current-buffer) nil catching?-hash)
         (signal (car err) (cdr err))))
      (when-let (dir (gethash (current-buffer) new-dirs-hash))
        (setq-local default-directory dir))
      (puthash (current-buffer) nil new-dirs-hash)
      (puthash (current-buffer) nil catching?-hash)
      result)))

(cl-defun command-palette:execute (&optional expr)
  "TODO"
  (interactive)
  (require 'wand)
  (when (null expr)
    (error "command-palette: No expression to execute"))
  (let* ((main-window (or (window-parameter (selected-window) :local/main-window)
                          (selected-window)))
         (main-buffer (if (boundp 'local/main-buffer)
                          local/main-buffer
                        (current-buffer)))
         (dir default-directory))
    (with-selected-window main-window
      (with-current-buffer main-buffer
        (unless (or (null expr) (string-empty-p expr))
          (command-palette:call-with-current-dir
           dir
           #'(lambda ()
               (funcall command-palette:*exec-fn* expr))))))))

(define-minor-mode command-palette-mode
  "Global mode for command palette feature."
  :lighter " CP"
  :global t
  (if command-palette-mode
      (command-palette:enable)
    (command-palette:disable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Done: How to properly resize the header window
;; (fit-window-to-buffer)

;; Done: Set default-directory -> See Blank buffer

;; Done: Check if the command-palette window exists

;; Done: Setup hook -> Whenever the main buffer is switched -> Reset current buffer

;; TODO: Lock the window and buffer, i.e. window and buffer cannot be manually resized, cannot be killed
;; (set-window-dedicated-p (get-buffer-window) t)
;; (emacs-lock-mode 'kill)

;; Done: Set a different font for the command-palette window?

;; TODO: Prevent the main window from being killed if it's the only one that is not a tool buffer window

;; TODO: Kill the window when the main buffer window is killed

;; Irrelevant: Kill the buffer when the main buffer is killed

;; TODO: Remove the redundant keyboard shortcut for ~exec*

;; Done: Programmatically create this window

;; Done: Advice after visiting a file

;; Done: Advice after a buffer is killed

;; TODO: Default content for this buffer

;; TODO: Custom content for this buffer

;; TODO: Prevent the buffer from being scrolled?

;; TODO: Auto-save?

;; TODO: Doc: (posn-point (event-end last-command-event))

;; TODO: Dealing with horizontal split

;; TODO: Dealing with multiple frames viewing the same buffer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring command-palette feature")

(provide 'rmacs:config-module-command-palette)
