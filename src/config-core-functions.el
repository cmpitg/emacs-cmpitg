;; -*- lexical-binding: t -*-

;;
;; Copyright (C) 2018-2024 Ha-Duong Nguyen (@cmpitg)
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

;; TODO: Check: async-start-process → ~exec-|-async

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~delete-output-block ()
  "Removes the current output block."
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (call-interactively #'~mark-current-output-block)
    (delete-region (region-beginning) (region-end))))

(cl-defun ~show-buffer-chooser ()
  "Shows the buffer chooser tool."
  (interactive)
  (require 'rmacs:config-module-simple-buffer-list)
  (call-interactively #'bl:show-buffer-chooser))

(defun ~deconstruct-path (path)
  "Deconstructs a path notation into `path' and `number' or
`pattern'.  See the following examples for further information:

\(~deconstruct-path \"/tmp/aoeu\"\)                       ⇒ \(values \"/tmp/aoeu\"\)
\(~deconstruct-path \"/tmp/aoeu:10\"\)                    ⇒ \(values \"/tmp/aoeu\" 10\)
\(~deconstruct-path \"/tmp/aoeu:10/other/:20\"\)          ⇒ \(values \"/tmp/aoeu:10/other/:20\" 20\)
\(~deconstruct-path \"/tmp/aoeu:/hello world/\"\)         ⇒ \(values \"/tmp/aoeu\" \"hello world\"\)
\(~deconstruct-path \"/tmp/aoeu:/inside:/hello world/\"\) ⇒ \(values \"/tmp/aoeu:/inside\" \"hello world\"\)
"
  (let ((matches (or (~string-match (rx (group (one-or-more any))
                                        ":" (group (one-or-more digit))
                                        eol)
                                    path)
                     (~string-match (rx (group (one-or-more any))
                                        ":/" (group (one-or-more any)) "/" eol)
                                    path))))
    (if matches
        (let* ((path (nth 1 matches))
               (pattern-or-number (nth 2 matches))
               (number (string-to-number pattern-or-number)))
          (if (zerop number)
              (values path pattern-or-number)
            (values path number)))
      (values path))))

(defun ~insert-exec ()
  "Inserts an executable from PATH or the current working
directory to the current buffer."
  (interactive)
  ;; TODO: Save history
  (defvar *~recent-inserted-execs* (list)
    "List of recently inserted executables.")
  (let* ((execs (s-lines (~exec-sh "get-all-execs")))
         (current-dir-execs (thread-first (~exec-sh (list "find" "." "-type" "f" "-maxdepth" "1"))
                              (s-lines)
                              (butlast)))
         (all-execs (append *~recent-inserted-execs* current-dir-execs execs))
         (output (ivy-read "Choose exec: " all-execs
                           :history '*~recent-inserted-execs*)))
    (unless (s-blank? output)
      (insert output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window & Frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-frames-mru ()
  "Gets the list of frames is most-recently-used order."
  (sort (frame-list)
        #'(lambda (x y)
            (> (or (frame-parameter x :custom/focus-time) 0)
               (or (frame-parameter y :custom/focus-time) 0)))))

(defun ~get-previous-frame ()
  "Gets the previously focused frame that is not the current one."
  (if (= 1 (length (frame-list)))
      nil
    (cl-loop for frame in (~get-frames-mru)
             unless (equalp frame (selected-frame))
             return frame)))

(defun ~toggle-sticky-window ()
  "Toggles stickiness of the currently active window."
  (interactive)
  (let* ((window (get-buffer-window (current-buffer)))
         (sticky? (window-dedicated-p window)))
    (set-window-dedicated-p window (not sticky?))
    (message (if (not sticky?)
                 "Window '%s' is now sticky"
               "Window '%s' is now normal")
             (current-buffer))))

(cl-defun ~toggle-toolbox (&key path
                                (side 'right)
                                (size -78)
                                (follow-dir t))
  "Toggles toolbox file.  The path to the toolbox file is passed
on using the `path' argument.  The toolbox window is sticky,
appears on the `side', and using `size' as its width.
`follow-dir' determines whether or not the toolbox buffer
inherits the working directory from the buffer that calls it.
Returns the toolbox window."
  (interactive)
  (when (null path)
    (error "PATH must be set to a path"))
  ;; TODO: Correctly get the buffer
  (let ((toolbox-buffer (get-file-buffer path))
        (working-dir default-directory))
    (if-let (window (and toolbox-buffer
                         (get-buffer-window toolbox-buffer)))
        (delete-window window)
      (progn
        (split-window (selected-window) size side)
        (with-selected-window (windmove-do-window-select side)
          (let ((split-window-preferred-function nil)
                (pop-up-windows nil))
            (~smart-open-file path))
          (when follow-dir (setq-local default-directory working-dir))
          (set-window-dedicated-p (selected-window) t)
          (selected-window))))))

(defun ~set-pop-up-buffer-mode (mode)
  "Sets mode for pop-up buffer.  MODE should either be :WINDOW or :FRAME."
  (cl-case mode
    (:window
     (custom-set-variables `(*popup-buffer-in* :window)
                           `(display-buffer-alist nil))

     ;; Prefer creating a new window
     ;; (custom-set-variables `(pop-up-windows t))

     ;; Prefer reusing the current window
     (custom-set-variables `(pop-up-windows nil)))
    (:frame
     (custom-set-variables `(*popup-buffer-in* :frame))

     ;; To make the behavior of `display-buffer' consistent, do not allow it
     ;; to split/create a new window by setting to `nil'
     (setq pop-up-windows nil)

     ;; Display some buffers in a separate frame so that they don't steal the
     ;; current window
     (dolist (buffer-regex (list (rx bol "*Help*" eol)
                                 (rx bol "*compilation*" eol)
                                 (rx bol "*cider-result*" eol)
                                 (rx bol "*cider-doc*" eol)
                                 (rx bol "*Org Agenda*" eol)
                                 (rx bol "*ivy-occur")))
       (add-to-list 'display-buffer-alist (cons buffer-regex (cons #'special-display-popup-frame nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File & buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defalias '~switch-buffer 'ivy-switch-buffer
;;   "Switches to a buffer and focus the corresponding window & frame.")

(defalias '~switch-buffer '~show-buffer-chooser
  "Switches to a buffer and focus the corresponding window & frame.")

;; FIXME: Review these hippie-expand enhancements
;; (defun try-expand-flexible-abbrev (old)
;;   "Tries to complete word using flexible matching.

;; Flexible matching works by taking the search string and then
;; interspersing it with a regexp for any character. So, if you try
;; to do a flexible match for `foo' it will match the word
;; `findOtherOtter' but also `fixTheBoringOrange' and
;; `ifthisisboringstopreadingnow'.

;; The argument OLD has to be nil the first call of this function, and t
;; for subsequent calls (for further possible completions of the same
;; string).  It returns t if a new completion is found, nil otherwise."
;;   (unless old
;;     (he-init-string (he-lisp-symbol-beg) (point))
;;     (if (not (he-string-member he-search-string he-tried-table))
;;         (setq he-tried-table (cons he-search-string he-tried-table)))
;;     (setq he-expand-list
;;           (and (not (equal he-search-string ""))
;;                (he-flexible-abbrev-collect he-search-string))))

;;   (while (and he-expand-list
;;               (he-string-member (car he-expand-list) he-tried-table))
;;     (setq he-expand-list (cdr he-expand-list)))

;;   (cond ((null he-expand-list)
;;          (if old
;;              (he-reset-string))
;;          nil)
;;         (t
;;          (he-substitute-string (car he-expand-list))
;;          (setq he-expand-list (cdr he-expand-list))
;;          t)))

;; (defun he-flexible-abbrev-collect (str)
;;   "Find and collect all words that flex-matches STR.
;; See docstring for `try-expand-flexible-abbrev' for information
;; about what flexible matching means in this context."
;;   (let ((collection nil)
;;         (regexp (he-flexible-abbrev-create-regexp str)))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp regexp nil t)
;;         ;; Is there a better or quicker way than using `thing-at-point' here?
;;         (setq collection (cons (thing-at-point 'word) collection))))
;;     collection))

;; (defun he-flexible-abbrev-create-regexp (str)
;;   "Generate regexp for flexible matching of STR.
;; See docstring for `try-expand-flexible-abbrev' for information
;; about what flexible matching means in this context."
;;   (concat "\\b" (mapconcat (lambda (x) (concat "\\w*" (list x))) str "")
;;           "\\w*" "\\b"))

(defun ~get-buffer-list ()
  "Gets the current buffer list, ordered by last visited."
  (lexical-let ((res (iflipb-interesting-buffers)))
    (concatenate 'list
                 (iflipb-interesting-buffers)
                 (cl-loop for b in (buffer-list)
                          unless (member b res)
                          collect b))))

(defun ~get-current-buffer-index ()
  "Gets index of the current buffer in the buffer list returned
by `~get-buffer-list'."
  iflipb-current-buffer-index)

(defun ~toggle-scratch ()
  "Toggles the `scratch.el' buffer.  `scratch.el' should reside
in the `*scratch-dir*' directory."
  (interactive)
  (let* ((scratch-dir (or *scratch-dir* temporary-file-directory))
         (scratch-file (s-concat scratch-dir "scratch.el")))
    (~toggle-toolbox :path scratch-file :size -80)))

;; TODO: Thinking
(cl-defun ~setup-temp-buffer (&optional (buffer (current-buffer)))
  "Sets up a temporary buffer."
  (interactive)
  (with-current-buffer buffer
    (with-eval-after-load "evil"
      (when evil-mode
        (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer)))
    (bind-key "q" #'kill-current-buffer (current-local-map))))

(cl-defun ~popup-buffer-frame (&key (buffer "*Temp*")
                                    content
                                    working-dir)
  "Pops up a buffer in a new frame, useful for workflows with
tiling window manager.  If CONTENT is non-nil, it serves as the
content of the buffer.  When the buffer is closed, the
corresponding frame is deleted."
  (interactive)
  (let* ((buffer (get-buffer-create buffer)))
    (with-current-buffer buffer
      (when content
        (~clean-up-buffer)
        (insert content)
        (with-eval-after-load "evil"
          (evil-define-key 'normal 'local (kbd "q") #'kill-current-buffer))
        (bind-key "q" #'kill-current-buffer (current-local-map)))

      (switch-to-buffer-other-frame buffer)
      (setq-local default-directory working-dir)
      (setq-local local/delete-frame-on-close (selected-frame)))))

;; TODO: Check if keymap exists
(cl-defun ~popup-buffer-window (&key (buffer "*Temp*")
                                     content
                                     working-dir
                                     (size 80))
  "Pops up a buffer in a new window.  If CONTENT is non-nil,
it serves as the content of the buffer.  SIZE defines the width
threshold which the window receives.  When the buffer is closed,
the corresponding window is deleted."
  (interactive)
  (let ((buffer (get-buffer-create buffer)))
    ;; Make sure the input window doesn't exist in any frame
    (when-let (wind (get-buffer-window buffer t))
      (delete-window wind))

    ;; Now create the window
    (with-current-buffer buffer
      (when content
        (~clean-up-buffer)
        (insert content))

      (split-window (selected-window) size 'left)
      (setq-local default-directory working-dir)
      (setq-local local/delete-window-on-close t)
      (switch-to-buffer buffer))
    buffer))

(cl-defun ~popup-buffer (&key (buffer "*Temp*")
                              content
                              working-dir
                              (size 80))
  "Pops up a buffer in a new window or frame.  If CONTENT is non-nil,
it serves as the content of the buffer.  SIZE defines the width
threshold which the window receives in case a new window is
popped up.  When the buffer is closed, the corresponding
window/frame is deleted.

This function will pop up a buffer window if the variable
*POPUP-BUFFER-IN* is :window and it is called without a prefix
argument.  Otherwise, it will pop up a buffer frame."
  (interactive)
  (cond
   ((or (eq :frame *popup-buffer-in*)
        current-prefix-arg)
    (~popup-buffer-frame :buffer buffer
                         :content content
                         :working-dir working-dir))
   ((eq :window *popup-buffer-in*)
    (~popup-buffer-window :buffer buffer
                          :content content
                          :working-dir working-dir
                          :size size))
   (t
    (error "Unrecognized value of *popup-buffer-in*: %s. It must be either :window or :frame."
           *popup-buffer-in*))))

;; (defalias '~popup-buffer 'internal-temp-output-buffer-show
;;   "Pops up a buffer for temporary display.")

(defun ~new-buffer-frame-from-selection ()
  "Opens a new frame with a temporary buffer containing the
  current selection."
  (interactive)
  (~popup-buffer-frame :content (~get-selection)
                       :working-dir default-directory))

;; TODO: Remove the 'open with' logic, replacing it with dispatch-action?
(cl-defun ~smart-open-file (path &key (new-frame? nil))
  "Opens path and with external program if necessary.  `path' is
expanded using `expand-file-name', then
`substitute-in-file-name'."
  (let ((path (string-trim (expand-file-name path))))
    (cl-dolist (regexp&action
                (append (if (boundp '*open-with-regexps*)
                            *open-with-regexps*
                          (list))
                        (list `(".*" . (lambda (path)
                                         (~open-file-specialized path
                                                                 :new-frame? ,new-frame?))))))
      (let ((regexp (car regexp&action))
            (action (cdr regexp&action)))
        (when (~string-matches? regexp path)
          (cl-return (cl-typecase action
                       (function   (funcall action path))
                       (string     (~open-with path action))
                       (otherwise  (error "Invalid program %s" action)))))))))

(defun ~gui/open-file ()
  (interactive)
  (let ((path (thread-first "zenity --file-selection --multiple --filename=%s 2>/dev/null"
                (format (thread-first default-directory
                          file-name-as-directory
                          shell-quote-argument))
                ~exec
                string-trim)))
    (unless (string-empty-p path)
      (find-file path))))

(defun ~gui/save-as ()
  (interactive)
  (let ((path (thread-first "zenity --file-selection --save --confirm-overwrite --filename=%s 2>/dev/null"
                (format (thread-first default-directory
                          file-name-as-directory
                          shell-quote-argument))
                ~exec
                string-trim)))
    (unless (string-empty-p path)
      (write-file path nil))))

(defun ~web-browse-gui (uri)
  "Calls a GUI browser on a URI."
  (interactive "MURI: ")
  (~open-with uri "web-browser-gui %s"))

(defun ~open-with-google-chrome (uri)
  "Opens a URI with Google Chrome."
  (interactive "MURI: ")
  (~open-with uri "run-chrome %s"))

(defun ~open-with (path program)
  "Opens path with `program'.  The `path' path is quoted
automatically."
  (~run-process (format program (shell-quote-argument path))))

(defun ~find-file-in-previous-frame (path)
  "Visits a file in the previous frame, or creates a new frame
then visits if there is no previous frame."
  (interactive "GPath: ")
  (when-let (frame (~get-previous-frame))
    (lexical-let ((dir default-directory))
      (select-frame frame)
      (find-file (f-join dir path)))))

;; TODO - RIGHT NOW
;; Review
;; ~counsel-grep-default-project-root

(cl-defun ~open-file-specialized (file-pattern &key (new-frame? nil))
  "Opens a path and jumps to a line based on number or a the
first occurrence of a pattern.  E.g.

* If `file-pattern' is a path, open it;

* If `file-pattern' is of format \"<path>:<number>\", open the
  file and jump to the corresponding line number;

* If `file-pattern' is of format \"<path>:/<pattern>/\", open the
  file and jump to the first occurrence of `pattern'.
"
  (cl-multiple-value-bind (path pattern)
      (~deconstruct-path file-pattern)
    (if new-frame?
        (~find-file-new-frame path)
      (find-file path))
    (when pattern
      (cond ((numberp pattern)
             (goto-line pattern))
            (t
             (goto-char (point-min))
             (re-search-forward pattern))))
    path))

(defun ~open-toolbox ()
  "Opens toolbox file."
  (interactive)
  (~smart-open-file *toolbox-path*))

(defun ~parse-tramp-argument (connection-string)
  "Returns an alist with

* protocol
* username
* host
* port
* path

from a Tramp connection string.

E.g.

\(~parse-tramp-argument \"/ssh:cmpitg@192.168.1.4#3355:/etc/network/interfaces\"\)
;; =>
;; \(\(protocol . \"ssh\"\)
;;  \(username . \"cmpitg\"\)
;;  \(host . \"192.168.1.4\"\)
;;  \(port . \"3355\"\)
;;  \(path . \"/etc/network/interfaces\"\)\)

\(~parse-tramp-argument \"/home/cmpitg/tmp/tmp.txt\"\)
;; =>
;; \(\(protocol . \"\"\)
;;  \(username . \"cmpitg\"\)
;;  \(host . \"localhost\"\)
;;  \(port . \"\"\)
;;  \(path . \"/home/cmpitg/tmp/tmp.txt\"\)\)
"
  (if (not (string-match "@" connection-string))
      `((protocol . "")
        (username . ,user-login-name)
        (host     . "localhost")
        (port     . "")
        (path     . ,connection-string))
    (cl-flet ((get-path (host-and-path)
                        (if (string-match (rx (group "/" (1+ anything))) host-and-path)
                            (match-string 1 host-and-path)
                          "/tmp/"))
              (get-port (host-and-path)
                        (if (string-match (rx (1+ (not (any "\\")))
                                              "#"
                                              (group (1+ digit)))
                                          host-and-path)
                            (match-string 1 host-and-path)
                          "22"))
              (get-host (host-and-path)
                        (if (string-match (rx bol
                                              (group (1+ (not (any "#" ":")))))
                                          host-and-path)
                            (match-string 1 host-and-path)
                          "localhost")))

      (string-match "^/\\([^:]+\\):\\([^@]+\\)@\\(.*\\)$" connection-string)

      (let* ((protocol      (match-string 1 connection-string))
             (username      (match-string 2 connection-string))
             (host-and-path (match-string 3 connection-string))

             (host          (get-host host-and-path))
             (port          (get-port host-and-path))
             (path          (get-path host-and-path)))
        `((protocol . ,protocol)
          (username . ,username)
          (host     . ,host)
          (port     . ,port)
          (path     . ,path))))))

(defun ~open-current-file-as-admin ()
  "Opens the current buffer as *nix root.
This command works on `sudo` *nixes only."
  (interactive)
  (when buffer-file-name
    (let* ((parsed-data (~parse-tramp-argument buffer-file-name))
           (username  (alist-get 'username parsed-data))
           (host      (alist-get 'host parsed-data))
           (path      (alist-get 'path parsed-data))
           (port      (alist-get 'port parsed-data)))
      (find-alternate-file
       (if (string-empty-p port)
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

;; TODO: Is buffer-local-value necessary?

(defun ~maybe-delete-file-when-killing-buffer ()
  "Deletes current file when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-on-close)
             local/delete-on-close)
    (~delete-current-file)))

(defun ~maybe-delete-frame-when-killing-buffer ()
  "Deletes current frame when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-frame-on-close)
             local/delete-frame-on-close)
    (delete-frame local/delete-frame-on-close)))

(defun ~maybe-delete-window-when-killing-buffer ()
  "Deletes current window when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-window-on-close)
             local/delete-window-on-close)
    (delete-window)))

(defun ~maybe-make-current-file-executable ()
  "Checks for the hashbang and `chmod u+x`s current file if
needed."
  (interactive)
  (and
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (save-match-data
         (looking-at "^#!/"))))
   (not (file-executable-p buffer-file-name))
   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
   (revert-buffer)
   (message "%s saved as executable" buffer-file-name)))

(defun ~delete-linked-windows ()
  "Deletes the linked windows, which are store in the local
variable `local/linked-windows'."
  (interactive)
  (when (local-variable-p 'local/linked-windows)
    (dolist (window local/linked-windows)
      (unless (or (equalp window (get-buffer-window))
                  (not (window-live-p window)))
        (delete-window window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~eval-current-sexp ()
  "Evals the current enclosing sexp."
  (interactive)
  (require 'expand-region)
  (let ((current-point (point)))
    (call-interactively #'er/mark-outside-pairs)
    (let ((res (call-interactively #'~eval-region)))
      (prin1 res)
      (goto-char current-point)
      (setq deactivate-mark t)
      res)))

(defun ~eval-last-sexp-pp ()
  "Evals the the last sexp and pprints to a buffer."
  (interactive)
  (let ((content (pp-to-string (eval (pp-last-sexp) lexical-binding)))
        (buffer (get-buffer-create "*emacs-lisp-eval*")))
    (~popup-buffer :buffer buffer
                   :content content
                   :working-dir default-directory)))

(defun ~eval-then-replace-region-or-last-sexp ()
  "Evals then replaces region or last sexp with result."
  (interactive)
  (let ((value (~eval-last-sexp-or-region)))
    (if (~is-selecting?)
        (delete-region (region-beginning) (region-end))
      (kill-sexp -1))
    (insert (format "%s" value))))

;; RIGHT HERE

(cl-defun ~execute-current-wand-text ()
  "Executes current Wand text with `~execute'."
  (interactive)
  (end-of-thing 'wand-text)
  (~execute (string-trim (thing-at-point 'wand-text t))))

(cl-defun ~execute-pop-up (&optional thing
                                     &key
                                     (exec-fn #'wand:execute)
                                     (selection-fn #'~get-thing-to-execute-from-context))
  "Interactively executes THING which is a piece of text or an
sexp using `exec-fn' and pops up the result when done.  If THING
is not provided, calls and takes the return value of SELECTION-FN
as THING."
  (interactive)
  (lexical-let ((output-buffer (format "* output :: %s :: %s *"
                                       thing
                                       (~gen-filename)))
                (workdir default-directory)
                (result (~execute thing :exec-fn exec-fn
                                  :selection-fn selection-fn)))
    (~popup-buffer :buffer output-buffer
                   :content result
                   :working-dir workdir)))

(with-eval-after-load "evil"
  (cl-defun ~advice/evil-ret-execute (orig-fn &rest args)
    "Prevents `evil-ret' in Evil normal-mode and visual-mode to
move the cursor but rather to call `~execute'."
    (interactive)
    (require 'thingatpt)
    (cond ((evil-visual-state-p)
           (let ((thing (if (eq 'line (evil-visual-type))
                            (string-trim-right (thing-at-point 'line))
                          (~get-selection))))
             (~execute thing)))
          ((evil-normal-state-p)
           (lexical-let* ((current-point (point))
                          (res (apply orig-fn args)))
             (unless (= current-point (point))
               (goto-char current-point)
               (setq res (call-interactively #'~execute)))
             res))
          (t
           (apply orig-fn args)))))

(use-package thing-cmds)
(defun ~select-multiline-exec-text ()
  "Selects multiline exec text."
  (interactive)
  (mark-things 'exec-text))

(defun ~toggle-popup-exec-result ()
  "Toggles whether or not result from an exec function is popped
up in a separate frame."
  (interactive)
  (setf *~popup-exec-result?* (not *~popup-exec-result?*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~firefox (url &key (new-window? nil))
  "Opens a URL in Firefox."
  (interactive
   (list (read-string "URL: " (cond
                               ((~is-selecting?)
                                (~get-selection))
                               ((thing-at-point-url-at-point)
                                (thing-at-point-url-at-point))
                               (t
                                "https://encrypted.google.com/")))))
  (let ((url (shell-quote-argument url)))
   (~run-process (if new-window?
                     (format "firefox --new-window %s" url)
                   (format "firefox %s" url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process/Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ~exec-pop-up (command)
  "Executes a command asynchronously & pops up a temporary buffer
showing result when done.  COMMAND is a list of strings, denoting
the command.

This function returns corresponding asynch process."
  (interactive)
  (lexical-let ((command-str (thread-first (cl-loop for arg in command
                                                 collect (shell-quote-argument arg))
                               (string-join " ")
                               string-trim)))
    (~add-to-history-file *~exec-history-path* command-str
                          :max-history *~exec-history-max*)
    (~exec-async command
                 :output-callback #'(lambda (output-buffer)
                                      (with-current-buffer output-buffer
                                        (~ansi-colorize-buffer))
                                      (pop-to-buffer output-buffer))
                 :output-as-buffer t
                 :keep-output-buffer t)))

(cl-defun ~exec-sh-pop-up (command)
  "Similar to `~exec-pop-up' but COMMAND is a string denoting a
shell command.  This allows the use of shell operators in
COMMAND.  Note that shell arguments might need quoting, e.g. with
`shell-quote-argument'."
  (interactive "MCommand: ")
  (lexical-let ((command (string-trim command)))
    (~add-to-history-file *~exec-history-path* command
                          :max-history *~exec-history-max*)
    (~exec-pop-up (list shell-file-name "-c" command))))

(cl-defun ~exec-pop-up.old (command)
  "Executes a command & pops up a temporary buffer showing
result, returing the process.  The command is executed asynchronously."
  (interactive "MCommand: ")
  (let* ((name command)
         (process (start-process-shell-command name name command))
         (dir default-directory))
    (~popup-buffer :buffer name)
    (with-current-buffer name
      (setq-local default-directory dir)
      ;; For some weird reason, we need to sleep shortly before we're able to
      ;; jump to the beginning of the buffer
      (sleep-for 0 1)
      (goto-char (point-min)))
    process))

(defun ~exec-sh> (command)
  "Executes a *shell* command, taking input from the current region,
pops up a temporary buffer showing result, and returns the exit
code of the command."
  (interactive "MCommand: ")
  (~add-to-history-file *~exec-history-path* command
                        :max-history *~exec-history-max*)
  (prog1 (let ((inhibit-message t))
           (shell-command-on-region (region-beginning)
                                    (region-end)
                                    command

                                    ;; Output buffer name
                                    command

                                    ;; Don't replace current region
                                    nil

                                    ;; Error piped to output
                                    nil))
    (save-mark-and-excursion
      (with-current-buffer command
        (ansi-color-apply-on-region (point-min)
                                    (point-max))))
    (~popup-buffer :buffer command)))

(defun ~exec-current-block ()
  "Execs the current code block."
  (interactive)
  (save-mark-and-excursion
    (call-interactively #'~mark-current-block)
    (~exec-sh> "exec-stdin")))

;; (defmacro ~dispatch-action (&rest args)
;;   "Dispatches action based on args.  Ignore output."
;;   (interactive)
;;   `(progn
;;      (message-box "Dispatching action: %s; Workdir: %s" ,args ,(pwd))
;;      (~exec-|-async ("setsid" "--fork" "dispatch-action" ,@args)))
;;   ;; `(~exec-|-async ("setsid" "--fork" "dispatch-action" ,@args))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core functions")

(provide 'rmacs:config-core-functions)
