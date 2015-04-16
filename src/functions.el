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

(require 'cl)

(defun ~helm-grep ()
  "C-u helm-do-grep"
  (interactive)
  (let ((current-prefix-arg 1))
    (call-interactively 'helm-do-grep)))

(defun toolbox:open-file (path)
  "Open path and open with external program if necessary."
  (condition-case description
      (progn
        (find-file path))))

(defun toolbox:execute-and-replace ()
  "Execute command on selection using `wand:execute' then replace
selection with command output."
  (interactive)
  (let* ((text (save-excursion
                (get-selection)))
         (output (wand:execute text)))
    (call-interactively 'kill-region)
    (insert output)))

(defun ~toggle-split-method ()
  "Toggle default split method between vertical split and
horizontal split."
  (interactive)
  (cond (split-width-threshold
         (setq *split-width-threshold-old* split-width-threshold)
         (setq split-width-threshold nil))
        (t
         (setq split-width-threshold *split-width-threshold-old*))))

;;;
;;; TODO: Document me
;;;

;; (put 'font-lock-add-keywords 'lisp-indent-function 1)

;; ;;; Better `if' and `list' indentation
;; (put 'list 'lisp-indent-function nil)
;; (put 'if 'lisp-indent-function 1)
;; (put 'quote lisp-indent-function 1)

(defalias 'qrr 'query-replace-regexp)
(defalias 'sr 'search-forward-regexp)
(defalias 'vtt 'visit-tags-table)
(defalias 'cr 'create-tags)
(defalias 'ib 'ibus-mode)
(defalias 'rb 'revert-buffer)
(defalias 'sbke 'save-buffers-kill-emacs)
(defalias 'tw 'twit)
(defalias 'bs 'bookmark-save)
(defalias 'am 'auto-complete-mode)
(defalias 'fm 'folding-mode)

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

(defun ~current-line-number ()
  "Return current line number in the buffer."
  (interactive)
  (count-lines 1 (point)))

(defun ~current-line-comment-syntax ()
  "Return the current line-comment syntax for current buffer mode."
  comment-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sticky window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~one-window ()
  "Delete all other non-dedicated windows."
  (interactive)
  (mapcar '(lambda (window)
             (unless (window-dedicated-p window)
               (delete-window window)))
          (cdr (window-list))))

(defun ~delete-window ()
  "Delete current window if it's not sticky/dedicated.  Use
prefix arg (`C-u') to force deletion if it is."
  (interactive)
  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Window '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))
      (delete-window (selected-window))))

(defun ~toggle-sticky-window ()
  "Toggle stickiness of the currently active window."
  (interactive)

  (let* ((window  (get-buffer-window (current-buffer)))
         (sticky? (window-dedicated-p window)))
    (set-window-dedicated-p window (not sticky?))
    (message (if (not sticky?)
                 "Window '%s' is now sticky"
               "Window '%s' is now normal")
             (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun ~switch-to-scratch ()
  "Switch to the scratch.el in `*scratch-dir*' directory."
  (interactive)
  (unless (string-equal "scratch.el" (~current-buffer-name))
    (if (get-buffer "scratch.el")
        (switch-to-buffer-other-window "scratch.el")
      (find-file-other-window (s-concat *scratch-dir* "scratch.el")))))

(defun ~switch-to-scratch-common-lisp ()
  "Switch to the scratch.lisp in `*scratch-dir*' directory."
  (interactive)
  (unless (string-equal "scratch.lisp" (~current-buffer-name))
    (if (get-buffer "scratch.lisp")
        (switch-to-buffer-other-window "scratch.lisp")
      (find-file-other-window (s-concat *scratch-dir* "scratch.lisp")))))

(defun ~helm-multi-occur-all ()
  "multi-occur in all buffers backed by files."
  (interactive)
  (helm-multi-occur
   (delq nil
         (mapcar (lambda (b)
                   (when (buffer-file-name b) (buffer-name b)))
                 (buffer-list)))))

;;; TODO
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

(defun ~new-buffer ()
  "Open a new empty buffer.  Thanks Xah Lee for this function.
Reference:
http://ergoemacs.org/emacs/emacs_new_empty_buffer.html"
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))

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

(defun ~read-simplified-sexp-as-string (prompt)
  "Read a sexp from minibuffer with completion.  The sexp doesn't
need its top-level brackets.  This function returns a string."
  (interactive)
  (let ((minibuffer-completing-symbol t))
    (read-from-minibuffer prompt
                          nil
                          read-expression-map
                          nil
                          'read-expression-history)))

(defun ~eval-string (str)
  "Eval a string."
  (interactive "sString: ")
  (with-temp-buffer
    (insert "(progn \n")
    (insert str)
    (insert ")")
    (end-of-buffer)
    (call-interactively 'eval-last-sexp)))

(defun ~eval-then-replace-last-exp ()
  "Eval region then replace last expression with result."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun ~eval-then-replace ()
  "Eval region then replace region with result."
  (interactive)
  (let ((value (~eval-string (get-selection))))
    (kill-region (selection-start) (selection-end))
    (insert (format "%s" value))))

(defun ~insert-into-emacs-lisp-docstring (string)
  "Interactive command.  Prompt and insert a string and escape it
as Emacs Lisp docstring format.

E.g.

\(-insert-into-emacs-lisp-docstring \"\(message \\\"hola mundo!\\\"\)\"\)
"
  (interactive "MInput your string: ")
  (insert (s-replace-all '(("\\" . "\\\\\\\\")
                           ("(" . "\\\\(")
                           (")" . "\\\\)")
                           ("\"" . "\\\\\""))
                         string)))

(defun ~add-bracket-and-eval (&optional string)
  "Add outer-most surrounding bracket if necessary and eval the
string.  This function may be called interactively.  If it's in
interactive mode and there's current a selection, the selection
is evaluted.

This function is convenient when being called interactively or
quickly eval a selection which contains Emacs Lisp code.

E.g.

\(-add-bracket-and-eval \"message \\\"¡Hola mundo!\\\"\"\)
;; => ¡Hola mundo!

\(-add-bracket-and-eval \"\(message \\\"¡Hola mundo!\\\"\)\"\)
;; => ¡Hola mundo!
"
  (interactive)
  (let* ((preprocessed-sexp (cond ((not (~string-empty? string))
                                   string)
                                  ((is-selecting?)
                                   (get-selection))
                                  (t
                                   (~read-simplified-sexp-as-string "Command: "))))
         (sexp (if (not (and (s-starts-with? "(" preprocessed-sexp)
                             (s-ends-with?   ")" preprocessed-sexp)))
                 (format "(%s)" preprocessed-sexp)
                 preprocessed-sexp)))
    (~eval-string sexp)))

(defun ~add-load-path (path)
  "Add path to load-path."
  (add-to-list 'load-path path))

(defun ~save-macro (name)
  "Take a name as argument and save the last defined macro."
  (interactive "SName of the macro: ")
  (kmacro-name-last-macro name)                ; Use this name for the macro
  (find-file *saved-macro-path*)               ; Load the macro file
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)                      ; Copy the macro
  (newline)
  (save-buffer)
  (kill-buffer))

(defun ~is-var-defined? (symbol)
  "Check if the variable corresponding to the symbol is defined.

E.g.

\(~is-var-defined? 'a-random-symbol-unlikely-to-be-defined\)  ; => nil
\(~is-var-defined? 'c-mode-map\)                              ; => t"
  (boundp symbol))

(defun ~is-function-defined? (symbol)
  "Check if the function corresponding to the symbol is defined.

E.g.
\(~is-function-defined? 'a-random-symbol-unlikely-to-be-defined\)  ; => nil
\(~is-function-defined? '$is-function-defined?\)                   ; => t"
  (fboundp symbol))

(defmacro ~nonnil-or-get-selection-or (var &rest body)
  "Short form for

\(cond \(var
       var\)
      \(\(~is-selecting?\)
       \(~get-selection\)\)
      \(t
       body\)\)

Useful when building interactive functions.

E.g.

\(defun _say-something \(&optional var\)
  \(interactive\)
  \(let \(\(var \(~nonnil-or-get-selection-or var
                                          \(read-string \"Var? \"\)\)\)\)
    \(message-box \"%s\" var\)\)\)
"
  `(cond (,var
          ,var)
         ((~is-selecting?)
          (~get-selection))
         (t
          ,@body)))

(defun* ~read-string (prompt &key
                             (initial-input         nil)
                             (history               nil)
                             (default-value         nil)
                             (inherit-input-method  nil))
  "An alias of read-string, with keyword arguments.  See
`read-string' documentation for more details.

  Read a string from the minibuffer."
  (read-string prompt
               initial-input
               history
               default-value
               inherit-input-method))

(defun ~emacs-lisp-make-function ()
  "Make a `defun` with current selection at the end of the file."
  (interactive)
  (if (is-selecting?)
    (save-excursion
      (let ((function-name (get-selection))
            (*defun-template* (if (~is-var-defined? '*defun-template*)
                                *defun-template*
                                "
\(defun %s \(\)
  \"\"
  \)
")))
        (~insert-text-at-the-end (format *defun-template* function-name))))))

(defun* ~byte-compile-dir (&optional dir &key (force t))
  "Byte compile all Emacs Lisp files is `dir'."
  (interactive)
  (let* ((dir (cond (dir
                     dir)
                    (t
                     (read-directory-name "Directory: ")))))
    (cond (force 
           (byte-recompile-directory (expand-file-name dir) 0 t))
          (t
           (byte-recompile-directory (expand-file-name dir) 0 nil)))))

(defun ~rebuild-my-config ()
  "Rebuild my config by loading `*config-dir*/build-config.el`."
  (interactive)
  (~load-config-files "build-config.el"))

(defun ~alist-get (alist key)
  "Return just the value associated with the key in an alist."
  (cdr (assoc key alist)))

(defun ~get-library-full-path (library-name)
  "Return the full path to a library."
  (save-excursion
    (find-library library-name)
    (let ((file-path (~current-file-full-path)))
      (kill-buffer)
      file-path)))

(defun ~bind-key-temporary ()
  "Interactive command for `bind-key'."
  (interactive)
  (let* ((key-binding (read-key-sequence "Key sequence: "))
         (symbol      (~read-simplified-sexp-as-string "Eval: "))
         (fn          `(lambda ()
                         (interactive)
                         (~add-bracket-and-eval ,symbol))))
    (bind-key (format-kbd-macro key-binding) fn)))

(defun ~add-personal-keybinding (key-binding symbol)
  "Add a key binding to `bind-key' library's
`personal-keybindings'."
  (when (featurep 'bind-key)
    (let ((key-binding (list key-binding))
          (symbol      symbol))
      (add-to-list 'personal-keybindings (list key-binding symbol nil)))))

(defalias 'insert-into-emacs-lisp-docstring '~insert-into-emacs-lisp-docstring)
(defalias 'add-bracket-and-eval '~add-bracket-and-eval)
(defalias 'add-load-path '~add-load-path)
(defalias 'save-macro '~save-macro)
(defalias 'is-var-defined? '~is-var-defined?)
(defalias 'is-function-defined? '~is-function-defined?)
(defalias 'nonnil-or-get-selection-or '~nonnil-or-get-selection-or)
(defalias 'eval-string '~eval-string)
(defalias 'eval-then-replace '~eval-then-replace)
(defalias 'eval-then-replace-last-exp '~eval-then-replace-last-exp)
(defalias '~eval-selection 'eval-region)
(defalias 'eval-selection '~eval-selection)
(defalias 'emacs-lisp-make-function '~emacs-lisp-make-function)
(defalias 'byte-compile-dir '~byte-compile-dir)
(defalias 'rebuild-my-config '~rebuild-my-config)
(defalias 'get-library-full-path '~get-library-full-path)
(defalias 'add-personal-keybinding '~add-personal-keybinding)
(defalias 'read-simplified-sexp-as-string '~read-simplified-sexp-as-string)

(defun ~eshell-history ()
  "Display eshell commands as with M-x.  The selected command is
added to the current eshell buffer."
  (interactive)
  (insert
   (ido-completing-read "Eshell history: "
                        (delete-dups
                         (ring-elements eshell-history-ring)))))

(defun ~switch-to-eshell-back-and-forth ()
  "Switch to eshell if current is not eshell, and switch to last
active buffer if current buffer is eshell."
  (interactive)
  (cond ((string-match-p "\\*.*eshell.*\\*" (~current-buffer-name))
         (~switch-to-last-buffer))
        (t
         (eshell))))

(defun ~execute-command-in-eshell (&optional command)
  "Execute a command in the only \*eshell\* buffer."
  (interactive)
  (let ((command (cond ((not (~string-empty? command))
                        command)
                       ((~is-selecting?)
                        (~current-selection))
                       (t
                        (read-string "Command: ")))))
    (with-current-buffer "*eshell*"
      (call-interactively 'end-of-buffer)
      (insert command)
      (call-interactively 'eshell-send-input))))

(defun ~execute-command-and-switch-to-eshell (&optional command)
  "Execute a command and then switch to \*eshell\* buffer."
  (interactive)
  (~execute-command-in-eshell command)
  (switch-to-buffer "*eshell*"))

(defun ~execute-command-and-popup-eshell (&optional command)
  "Execute a command and then popup the \*eshell\* buffer."
  (interactive)
  (~execute-command-in-eshell command)
  (with-current-buffer "*eshell*"
    (end-of-buffer))
  (~popup-buffer "*eshell*"))

(defun ~cd-and-switch-to-eshell (&optional path)
  "Change dir to `path' in eshell and jump to eshell buffer."
  (interactive)
  (let ((path (cond ((not (~string-empty? path))
                     path)
                    ((~is-selecting?)
                     (~current-selection))
                    (t
                     (read-directory-name "Path: ")))))
    (~execute-command-and-switch-to-eshell (s-concat "cd " path))))

(defun ~cd-current-buffer-dir-and-switch-to-eshell ()
  "Change dir to current dir of buffer and jump to eshell buffer.
If current buffer is not backed by file, switch to eshell."
  (interactive)
  (~cd-and-switch-to-eshell (~current-dir)))

(defalias 'eshell-history '~eshell-history)
(defalias 'switch-to-eshell-back-and-forth '~switch-to-eshell-back-and-forth)
(defalias 'execute-command-in-eshell '~execute-command-in-eshell)
(defalias 'execute-command-and-switch-to-eshell '~execute-command-and-switch-to-eshell)
(defalias 'cd-and-switch-to-eshell '~cd-and-switch-to-eshell)
(defalias 'cd-current-buffer-dir-and-switch-to-eshell '~cd-current-buffer-dir-and-switch-to-eshell)
(defalias 'execute-command-and-popup-eshell '~execute-command-and-popup-eshell)

(defun ~evil-define-key (key func)
  "Define keymap in all evil states."
  (define-key evil-normal-state-map key func)
  (define-key evil-insert-state-map key func)
  (define-key evil-visual-state-map key func)
  (define-key evil-replace-state-map key func)
  (define-key evil-operator-state-map key func)
  (define-key evil-motion-state-map key func))

(defun ~evil-undefine-helper ()
  "(Helper) Prevent evil from disabling a default Emacs kepmap."
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(defun ~evil-undefine-key (key)
  "(Helper) Prevent evil from disabling a default Emacs kepmap."
  (~evil-define-key key 'evil-undefine))

(defun ~toggle-evil-local ()
  "Toggle evil-mode for current buffer."
  (interactive)
  (if evil-local-mode
    (progn
      (evil-local-mode -1)
      (setq cursor-type 'bar))
    (evil-local-mode)))

(defalias 'evil-define-key '~evil-define-key)
(defalias 'evil-undefine-helper '~evil-undefine-helper)
(defalias 'evil-undefine-key '~evil-undefine-key)
(defalias 'toggle-evil-local '~toggle-evil-local)

(defun ~find-file-extended (&optional dir-path)
  "Find file with `fiplr' in a directory.  Symlinks are
followed."
  (interactive)
  (if (require 'fiplr nil nil)
    (let ((path (cond ((not (null dir-path))
                       dir-path)
                      ((~is-selecting?)
                       (~get-selection))
                      (t
                       (read-directory-name "Directory path: ")))))
      (fiplr-find-file-in-directory (file-chase-links path) fiplr-ignored-globs))
    (message "You need `fiplr' package to use this function.")))

(defun ~clean-up-tramp ()
  "Close all tramp connections and buffers."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

(defun ~write-to-file (filename content)
  "Write string to file."
  (with-temp-buffer
    (insert content)
    (write-file filename)))

(defun ~make-executable ()
  "chmod +x current file."
  (interactive)
  (and
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (save-match-data
         (looking-at "^#!"))))
   (not (file-executable-p buffer-file-name))
   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
   (revert-buffer)
   (message
    (concat "Saved as script: " buffer-file-name))))

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

(defun* ~download-file (url filepath &key (overwrite nil))
  "Download a file.

E.g.

;; Download, raise an error if the file exists
\($download-file \"https://raw.github.com/defunkt/gist.el/master/gist.el\"
		\"/tmp/gist.el\"\)
c
;; Download and overwrite if already exists
\($download-file \"https://raw.github.com/defunkt/gist.el/master/gist.el\"
		\"/tmp/gist.el\"
		:overwrite t\)"
  (interactive "MURL: \nFSave to: ")
  (url-copy-file url filepath overwrite))

(defun ~scm-status ()
  "Call for the corresponding SCM `status` command."
  (interactive)
  (let ((current-scm (~get-scm)))
    (cond
     ((string= "git" current-scm)
      (magit-status nil))

     ((string= "hg" current-scm)
      (monky-status))

     (t nil))))

(defun ~get-scm ()
  "Return the current source control management (SCM) of current
file as string."
  (interactive)
  (let ((mode-name (downcase
                    (replace-regexp-in-string " \\|[[:digit:]]\\|:.*\\|-.*" ""
                                              (or vc-mode "")))))
    (cond ((and (~string-empty? mode-name)
                (magit-get-top-dir))
           "git")
          (t
           mode-name))))

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

(defun ~open-file-gui ()
  "Open a file using Zenity."
  (interactive)
  (let ((filename (~string-but-last (~exec (~build-open-file-cmd-string)))))
    (unless (~string-empty? filename)
      (~open-file filename))))

(defun ~open-file-gui-other-window ()
  "Open a file using Zenity."
  (interactive)
  (let ((filename (~string-but-last (~exec (~build-open-file-cmd-string)))))
    (unless (~string-empty? filename)
      (~open-file-other-window filename))))

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
  "Delete the file associated with the current buffer.
Delete the current buffer too."
  (interactive)
  (let ((current-file (~current-file-full-path)))
    (when (yes-or-no-p (concat "Delete file: " current-file))
      (kill-buffer (current-buffer))
      (delete-file current-file)
      (message (concat "Deleted file: " current-file)))))

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

(defun ~build-open-file-cmd-string ()
  "Build a string used to execute an open-file dialog."
  (concat "zenity --file-selection --filename "
          (~current-dir)
          " 2>/dev/null"))

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

(defalias 'find-file-extended '~find-file-extended)
(defalias 'write-to-file '~write-to-file)
(defalias 'make-executable '~make-executable)
(defalias 'list-dir '~list-dir)
(defalias 'read-file '~read-file)
(defalias 'download-file '~download-file)
(defalias 'scm-status '~scm-status)
(defalias 'get-scm '~get-scm)
(defalias 'is-directory? '~is-directory?)
(defalias 'open-file-gui '~open-file-gui)
(defalias 'open-file-gui-other-window '~open-file-gui-other-window)
(defalias 'current-dir '~current-dir)
(defalias 'list-dir-full-path '~list-dir-full-path)
(defalias 'current-file-full-path '~current-file-full-path)
(defalias 'current-path '~current-file-full-path)
(defalias 'current-file-name '~current-file-name)
(defalias 'open-current-file-as-admin '~open-current-file-as-admin)
(defalias 'delete-current-file '~delete-current-file)
(defalias 'build-open-file-cmd-string '~build-open-file-cmd-string)

(defalias '~open-file 'find-file
  "Open a file")

(defalias '~open-file-other-window 'find-file-other-window
  "Open a file in other window")

(defalias '~file-exists? 'file-exists-p
  "Determine if a file exists")

(defalias 'rename-current-file '~rename-current-file)

(defun ht-to-alist* (table)
  "Deeply convert hashtable to alist.

E.g.

\(ht-to-alist* \(ht \('a \(ht \('b 'c\)\)\)\)\)  ;; => '\(\(a \(b . c\)\)\)
"
  (let ((result (ht-to-alist table)))
    (-map (lambda (key-val)
            (let ((key (car key-val))
                  (val (cdr key-val)))
             (if (hash-table-p val)
               (cons key (ht-to-alist* val))
               (cons key val))))
          result)))

(defun ~jekyll-add-last-updated ()
  "Add last_update timestamp with `date -R` format."
  (interactive)
  (save-excursion
    (goto-point (point-min))
    (if (re-search-forward "^last_updated:.*$")
        (replace-match (format "last_updated: %s"
                               (~string-but-last (~exec "date -R")))))))

(defalias 'jekyll-add-last-updated '~jekyll-add-last-updated)

(defun ~add-emacs-lisp-header ()
  "Add Emacs Lisp header."
  (interactive)
  (insert ";;; mylib.el --- 
;; 
;; Filename: mylib.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Copyright (C) 2014  Duong Nguyen
;; Created: $ date -R
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mylib.el ends here"))

(defun* ~add-gplv3-header (&key author
                               project-name
                               year)
  "Add GLPv3 header with

*  `author' as the copyright holder,
* `project-name' as the name of the project, and
* `year'

For each argument, if it's not provided, a prompt would appear
for you to input.

If `project-name' is an empty string, it means your file a
single-file project.  Thus the first line \"This file is a port
of ...\" would not be added to the header.

`$add-gplv3-header' knows to handle comments properly base on
variables `comment-start' and `comment-end'.

TODO: Handling comment when `comment-end' is not empty.
"
  (interactive)
  (let* ((author (if author
                   author
                   (read-string "Author: ")))
         (project-name (if project-name
                         project-name
                         (read-string "Project name: ")))
         (year (if year
                 year
                 (read-string "Year: ")))
         
         (project-name-exists? (if (or (null project-name)
                                       (= 0 (length project-name)))
                                 nil
                                 t))
         (this-file-or-project-name (if project-name-exists?
                                      project-name
                                      "This file"))
         (this-file-or-project-name-lowcase (s-downcase this-file-or-project-name))

         ;; Comment
         (comment (if (s-equals? comment-start ";")
                    ";;"
                    comment-start))

         (first-line (s-lex-format
                      "${comment} This file is part of ${project-name} project."))
         (gplv3-header-text (s-lex-format "${comment}
${comment} Copyright (C) ${year}  ${author}
${comment} 
${comment} ${this-file-or-project-name} is free software: you can redistribute it and/or modify
${comment} it under the terms of the GNU General Public License as published by
${comment} the Free Software Foundation, either version 3 of the License, or
${comment} \(at your option\) any later version.
${comment} 
${comment} ${this-file-or-project-name} is distributed in the hope that it will be useful,
${comment} but WITHOUT ANY WARRANTY; without even the implied warranty of
${comment} MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
${comment} GNU General Public License for more details.
${comment} 
${comment} You should have received a copy of the GNU General Public License
${comment} along with ${this-file-or-project-name-lowcase}.  If not, see <http://www.gnu.org/licenses/>.
${comment}
"))

         (header-text (if project-name-exists?
                        (s-lex-format "${comment}
${first-line}
${gplv3-header-text}")
                        gplv3-header-text)))
    (message-box "%s %s" project-name-exists? (length project-name))
    (insert header-text)
    (message "Done, you might want to reformat your header.")))

(defalias 'add-emacs-lisp-header '~add-emacs-lisp-header)
(defalias 'add-gplv3-header '~add-gplv3-header)

(defun ~start-emacs-server (&rest dir)
  "Start an Emacs server in a specific socket directory.  If no
directory is specified, the default dir /tmp/emacs1000/server is
used.  Do nothing if server is already started."
  (setq server-socket-dir (if dir
                            dir
                            "/tmp/emacs1000/server"))
  (unless (and (~is-var-defined? 'server-socket-dir)
               (file-exists-p server-socket-dir))
    (server-start)))

(defun ~clipboard<-region (begin end)
  "Copy region to clipboard."
  (clipboard-kill-ring-save begin end))

(defun ~kill-ring<- (str)
  "Copy a string to the kill ring."
  (interactive "MString: ")
  (kill-new str))

(defun ~clipboard<- (str)
  "Copy a string to clipboard."
  (interactive "MString: ")
  (let ((x-select-enable-clipboard t))
    (x-select-text str)))

(defun ~clipboard<-pwd ()
  "Copy current directory to clipboard."
  (interactive)
  (~clipboard<- (~current-dir)))

;; (defun ~copy-to-clipboard (text)
;;   "Copy a string to clipboard."
;;   (interactive "MString: ")
;;   (let ((tempfile (make-temp-file "emacs-clipboard")))
;;     (write-to-file tempfile text)
;;     ()
;;     )

;;   )

(defun ~copy-directory ()
  "Copy current directory to clipboard."
  (interactive)
  (~clipboard<- (~current-dir)))

(defun ~keyboard-quit ()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we
clicked away or set the cursor into another buffer) we can quit
by pressing 'ESC' three times. This function handles it more
conveniently, as it checks for the condition of not beign in the
minibuffer but having it active. Otherwise simply doing the ESC
or (keyboard-escape-quit) would brake whatever split of windows
we might have in the frame."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
    (if (or mark-active (active-minibuffer-window))
	  (keyboard-escape-quit))
    (keyboard-quit)))

(defun ~modify-opacity (&optional dec)
  "Modify the opacity of emacs frame; if DEC is t,
increase the opacity."
  (let* ((alpha-or-nil (frame-parameter nil 'alpha))
         (old-alpha (if alpha-or-nil alpha-or-nil 100))
         (new-alpha (if dec (- old-alpha 10) (+ old-alpha 10))))
    (when (and (>= new-alpha frame-alpha-lower-limit) (<= new-alpha 100))
      (modify-frame-parameters nil (list (cons 'alpha new-alpha))))))

(defun ~put-mode-line-to-top ()
  "Put the mode-line to the top of the window."
  (setq header-line-format mode-line-format mode-line-format nil))

(defun ~init-hyde (&optional path)
  "Place a sample Hyde's .hyde.el to your Jekyll blog."
  (interactive)
  (let ((path (cond (path
                     path)
                    (t
                     (read-directory-name "Path to your Jekyll: ")))))
    (ignore-errors
        (f-copy (~get-local-config-dir "samples/dot-hyde.el")
                (concat path ".hyde.el")))
    (~open-file (concat path ".hyde.el"))))

(defalias 'start-emacs-server '~start-emacs-server)
(defalias 'clipboard<-region '~clipboard<-region)
(defalias 'kill-ring<- '~kill-ring<-)
(defalias 'clipboard<- '~clipboard<-)
(defalias 'clipboard<-pwd '~clipboard<-pwd)
(defalias 'modify-opacity '~modify-opacity)
(defalias 'put-mode-line-to-top '~put-mode-line-to-top)

(defun ~setup-moz-javascript ()
  "Setting JavaScript mode with MozRepl."
  (moz-minor-mode 1))

(defun ~sunrise ()
  "Open Sunrise Commander, remove the nonpane buffer."
  (interactive)
  (unless sr-running
    (sunrise)
    (sr-reset-view-remove-nonpane-buffer)))

(defun ~sunrise-cd ()
  "Open Sunrise Commander with current directory, remove the
nonpage buffer."
  (interactive)
  (unless sr-running
    (sunrise-cd)
    (sr-reset-view-remove-nonpane-buffer)))

(defun sr-reset-view ()
  "Reset Sunrise Commander pane view."
  (interactive)
  (when sr-running
    (sr-setup-windows)))

(defun sr-reset-view-remove-nonpane-buffer ()
  "Reset Sunrise Commander pane view, removing the nonpane
buffer."
  (interactive)
  (when sr-running
    (sr-setup-windows)
    (windmove-down)
    (delete-window)))

(defun ~auto-load-mode (filetypes mode)
  "Autoload mode for filetype regex or a list of filetypes.

Example:

  \(~auto-load-mode \"\\\\.rake$\" 'ruby-mode\)
  \(~auto-load-mode '(\"\\\\.md$\" \"\\\\.markdown$\") 'markdown-mode\)
"
  (if (stringp filetypes)
    (add-to-list 'auto-mode-alist (cons filetypes mode))
    (dolist (filetype filetypes)
      (add-to-list 'auto-mode-alist (cons filetype mode)))))

(defun ~noweb-code-chunk-add-mode (mode-name)
  "Add mode to a code chunk"
  (interactive "MMode name (without `-mode`): ")
  (insert (concat "-*- mode: " mode-name " -*-")))

(defun ~compile-haml ()
  "Compile HAML file to HTML file."
  (interactive)
  (and (string-match ".*\.haml$" (~current-file-full-path))
       (let ((output-file (replace-regexp-in-string "\.haml$" ".html"
                                                    (~current-file-full-path))))
         (compile (concat "haml \"" (~current-file-full-path) "\" "
                          "\"" output-file "\"")))))

(defun ~compile-coffee ()
  "Compile CoffeeScript to JavaScript."
  (interactive)
  (and (string-match ".*\.coffee$" (~current-file-full-path))
       (compile (concat "coffee -c " (~current-file-full-path)))))

(defun ~compile-livescript ()
  "Compile LiveScript to JavaScript."
  (interactive)
  (and (string-match ".*\.ls$" (~current-file-full-path))
       (compile (concat "livescript -c -d " (~current-file-full-path)))))

(defun ~toggle-ibus ()
  "Toggle ibus."
  (interactive)

  (unless (~is-var-defined? '*is-ibus-on?*)
    (defvar *is-ibus-on?* nil))

  (if (null *is-ibus-on?*)
    (progn (ibus-enable)
           (setf *is-ibus-on?* t))
    (progn (ibus-disable)
           (setf *is-ibus-on?* nil))))

(defun ~toggle-ecb ()
  "Toggle ECB."
  (interactive)

  (unless (~is-var-defined? '*is-ecb-running?*)
    (defvar *is-ecb-running?* nil))

  (if (null *is-ecb-running?*)
    (progn (ecb-activate)
           (setf *is-ecb-running?* t))
    (progn (ecb-deactivate)
           (setf *is-ecb-running?* nil))))

(defun ~load-paredit-mode ()
  "Load paredit mode and disable autopair."
  (paredit-mode t)
  (when (~is-function-defined? 'autopair-mode)
    (autopair-mode 0))
  (when (~is-function-defined? 'smartparens-mode)
    (smartparens-mode 0)))

(defun ~activate-evil-local-mode ()
  "Activate evil local mode."
  (interactive)
  (toggle-evil-local)
  (setq-default cursor-type 'hbar)
  (setq-default cursor-type 'bar))

(defun ~markdown-outline-headings ()
  "Outline headings for current Markdown document using Helm."
  (interactive)
  (let ((helm-multi-occur-buffer-list (list (buffer-name (current-buffer)))))
    (helm-occur-init-source)
    (helm :input "^\\#"
          :sources 'helm-source-occur
          :buffer "*helm occur*"
          :history 'helm-grep-history
          :truncate-lines t)))

(defun ~markdown-make-inline-link ()
  "Turn `text1|text2` to Markdown's inline link format:
`[text1](text2)`."
  (interactive)
  (let* ((text (get-selection))
         (parts (s-split "|" text))
         (name     (first parts))
         (link-to  (s-join "|" (rest parts))))
    (replace-selection  (format "[%s](%s)" name link-to))))

(defalias 'setup-moz-javascript '~setup-moz-javascript)
(defalias 'auto-load-mode '~auto-load-mode)
(defalias 'noweb-code-chunk-add-mode '~noweb-code-chunk-add-mode)
(defalias 'compile-livescript '~compile-livescript)
(defalias 'compile-coffee '~compile-coffee)
(defalias 'compile-haml '~compile-haml)
(defalias 'toggle-ibus '~toggle-ibus)
(defalias 'toggle-ecb '~toggle-ecb)
(defalias 'load-paredit-mode '~load-paredit-mode)
(defalias 'markdown-make-inline-link '~markdown-make-inline-link)
(defalias 'markdown-outline-headings '~markdown-outline-headings)

(defun ~parse-tramp-argument (connection-string)
  "Return an alist with

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
    (flet ((get-path (host-and-path)
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

(defun ~el-get-package-list ()
  "Get the list of packages cached in el-get repositories.  This
function doesn't update el-get database.  Returns a plist of format

\(:name package-name :description package-description\)"
  (el-get-read-all-recipes))

(defun ~el-get-package-exists? (package-symbol)
  "Determine if a package exists in el-get repositories.  This
function doesn't update local el-get database."
  (not (null
        (memq package-symbol (-map (lambda (package)
                                     (plist-get package :name))
                                   (~el-get-package-list))))))

(defun ~remove-elpa-package (&optional package-name)
  "Remove an ELPA package from local directory.  You need to
`unload-feature' or restart Emacs for the changes to take effect.
`PACKAGE-NAME' does not need to contain version.  It's
automatically searched in your ~/.emacs.d/elpa/ directory.  This
function may be called as a command; user is prompted to input
full package name and its version."
  (interactive)
  (let ((path (cond ((not (~string-empty? package-name))
                     (-> (format "~/.emacs.d/elpa/%s*" package-name)
                       file-expand-wildcards
                       first))
                    (t
                     (read-directory-name "Package: "
                                          "~/.emacs.d/elpa/")))))
    (f-delete path t)))

(defun ~elpa-package-exists? (package-symbol)
  "Determine if a package exists in ELPA.  This function doesn't
update local ELPA database."
  (not (null (memq package-symbol (-map (lambda (element)
                                          (car element))
                                        (~elpa-get-package-list))))))

(defun ~elpa-get-package-list ()
  "Get the list of packages information cached in your ELPA repositories."
  package-archive-contents)

(defun ~elpa-get-installed-package-list ()
  "Get the list of packages information installed in your ELPA repositories.

This function return the value of `package-alist` variable. Which
returns an alist of all packages available for activation.

Each element has the form (PKG . DESC), where PKG is a package
name (a symbol) and DESC is a vector that describes the package.

The vector DESC has the form [VERSION-LIST REQS DOCSTRING].
  VERSION-LIST is a version list.
  REQS is a list of packages required by the package, each
   requirement having the form (NAME VL) where NAME is a string
   and VL is a version list.
  DOCSTRING is a brief description of the package."
  package-alist)

(defun ~elpa-install-packages (&rest packages)
  "Install package using Elpa if not installed."
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun ~el-get-install-packages (&rest packages)
  "Install package using el-get if not installed."
  (dolist (package packages)
    (unless (or (package-installed-p package)
                (el-get-package-is-installed package))
      (el-get-install package))))

(defun ~package-installed? (package-symbol)
  "Determine if a package is installed."
  (or (package-installed-p package-symbol)
      (el-get-package-is-installed package-symbol)
      (~local-package-is-installed? package-symbol)))

(defun ~local-package-is-installed? (package-symbol)
  "Determine if a local package is installed at
`*config-dir*/local-packages/`.  Note that this function works by
using feature name, not directory name."
  (memq package-symbol features))

(defun ~install-or-update-el-get ()
  "Install/update el-get."
  (interactive)
  (cond
   ((~is-function-defined? 'el-get-self-update)
    (el-get-self-update))

   (t
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp))))))

(defalias 'elpa-install-packages '~elpa-install-packages)
(defalias 'el-get-install-packages '~el-get-install-packages)
(defalias 'local-package-is-installed? '~local-package-is-installed?)
(defalias 'install-or-update-el-get '~install-or-update-el-get)

(defvar *shell-command-hist* '())

(defun ~popup-shell-command (&optional command)
  "Run a non-interactive shell command and popup a window to
display result."
  (interactive)
  (let* ((command-str (cond ((not (~string-empty? command))
                             command)
                            ((is-selecting?)
                             (get-selection))
                            (t
                             (~read-string "Shell command: "
                                           :history '*shell-command-hist*)))))
    (ignore-errors
      (get-buffer-create "*Shell Output*")
      (with-current-buffer "*Shell Output*"
        (erase-buffer)
        (insert "$ " command-str "\n"))
      (start-process-shell-command "Shell-Command" "*Shell Output*" command-str)
      (~popup-buffer "*Shell Output*"))))

(defun ~send-keys-to-tmux (&optional str)
  "Send key to tmux with Eshell."
  (interactive)
  (let ((keys (cond ((not (~string-empty? str))
                     str)
                    (t
                     (read-shell-command "Tmux: " nil '*shell-command-hist*)))))
    (~exec (format "tmux send-keys \"%s\" Enter" keys))))

(defun ~man-current-word ()
  "`man` this word."
  (interactive)
  (manual-entry (current-word)))

(defun ~exec (command)
  "Execute a shell command then return its value as string."
  (interactive "MCommand: ")
  (shell-command-to-string command))

(defun ~exec-in-other-window (command)
  "Execute in other window."
  (interactive "MCommand: ")
  (shell-command command))

(defun ~exec-then-pipe (command)
  "Execute and pipe output to the current buffer."
  (interactive "MCommand: ")
  (shell-command command t))

(defun ~exec-then-pipe-selection ()
  "Execute selection and pipe output to the current buffer."
  (interactive)
  (~exec-then-pipe (~current-selection)))

(defun ~pipe-then-exec (command)
  "Pipe current region to a command, exec it, and pipe the output back."
  (interactive "MCommand: ")
  (shell-command-on-region (if mark-active (region-beginning) 1)
                           (if mark-active (region-end) 1)
                           command t))

(defalias 'popup-shell-command  '~popup-shell-command)
(defalias 'man-current-word '~man-current-word)
(defalias 'exec '~exec)
(defalias 'exec-in-other-window '~exec-in-other-window)
(defalias 'exec-then-pipe '~exec-then-pipe)
(defalias 'exec-then-pipe-selection '~exec-then-pipe-selection)
(defalias 'pipe-then-exec '~pipe-then-exec)

(defalias '~filter-command '~pipe-then-exec
  "Filter a command")

(defalias '~pipe-then-exec-in-other-window 'shell-command-on-region
  "Filter a command but pipe the other to other window")

(defun* ~create-snippet (&optional mode
                                   &key
                                   filename
                                   mode
                                   abbrev
                                   short-description)
  "Create a snippet for `mode` and open that newly created
snippet.  The snippet is place inside my
`~/emacs-config/snippets/[mode]/[file-name]`.  Other arguments
are self-explanatory.  The snippet directory is created if it
doesn't exist yet."
  (interactive)
  (let* ((mode (if (string-empty? mode)
                   (~read-string "Mode: "
                                 :initial-input (format "%s" major-mode))
                 mode))
         (snippet-mode-dir (f-expand (format "~/emacs-config/src/snippets/%s" mode)))

         (abbrev (if (string-empty? abbrev)
                     (read-string "Abbrev: ")
                   abbrev))

         (snippet-file (if (string-empty? filename)
                           (read-file-name "File name: "
                                           snippet-mode-dir
                                           abbrev)
                         filename))

         (short-description (if (string-empty? short-description)
                                (read-string "Short description: ")
                              short-description)))
    ;; Make sure snippet directory exists
    (unless (f-exists? snippet-mode-dir)
      (f-mkdir snippet-mode-dir))

    (write-to-file snippet-file
                   (s-concat "# -*- mode: snippet -*-\n"
                             "# name: " short-description "\n"
                             "# key: " abbrev             "\n"
                             "# group: " mode             "\n"
                             "# --\n"))
    (find-file snippet-file)))

(defalias 'create-snippet '~create-snippet)

(defun* ~string-start-with? (string substring &key (ignore-case nil))
  "Determine if a string starts with a substring.

E.g.

\(~string-start-with? \"config-default\" \"config-\"\)  ;; => t
\(~string-start-with? \"config-default\" \"Config-\"\)  ;; => nil"
  (s-starts-with? substring string ignore-case))

(defun* ~string-end-with? (string substring &key (ignore-case nil))
  "Determine if a string ends with a substring.

E.g.

\(~string-end-with? \"config-default\" \"default\"\)  ;; => t
\(~string-end-with? \"config-default\" \"Default\"\)  ;; => nil"
  (s-ends-with? substring string ignore-case))

(defun ~string-empty? (str)
  "Determine if a string is empty.  `nil' is treated as empty
string."
  (or (null str)
      (and (stringp str)
           (= 0 (length str)))))

(defun ~string-but-last (str)
  "Return a string with its last character removed."
  (if (~string-empty? str) ""
      (substring str 0 -1)))

(defun ~string-contains? (str substring)
  "Check if a string contains a substring."
  (not (null (string-match substring str))))

(defun ~symbol->string (symbol)
  "Convert a symbol to a string."
  (symbol-name symbol))

(defun ~string->symbol (string)
  "Convert a string into an uninterned symbol."
  (make-symbol string))

(defun ~join-strings (separator a-seq)
  "Join strings.  Works with any type of sequence and any data type as its element.

E.g.

\($join-strings \"|\" '\(\"a\" \"b\" \"c\"\)\) ; => \"a|b|c\"
\($join-strings \"|\" [1 \"b\" c]\) ; => \"1|b|c\""
  (-reduce (lambda (result element)
             (format "%s%s%s" result separator element))
           (-map (lambda (x) x) a-seq)))

(defun ~trim-spaces (text)
  "Trim spaces at the beginning and the end of a portion of
text."
  (while (and (not (~string-empty? text))
              (string= " " (~first-char-as-string text)))
    (setf text (substring text 1)))

  (while (and (not (~string-empty? text))
              (string= " " (~last-char-as-string text)))
    (setf text (substring text 0 (- (length text) 1))))
  text)

(defun ->string (exp)
  "Convert an expression to string with `format'."
  (format "%s" exp))

(defun ~first-char-as-string (str)
  "Return the first character of a string as string."
  (if (not (~string-empty? str))
    (substring str 0 1)
    ""))

(defun ~last-char-as-string (str)
  "Return the last character of a string as string."
  (if (not (~string-empty? str))
    (let ((len (length str)))
      (substring str (- len 1) len))
    ""))

(defalias 'string-empty? '~string-empty?)
(defalias 'string-start-with? '~string-start-with?)
(defalias 'string-end-with? '~string-end-with?)
(defalias 'string-empty? '~string-empty?)
(defalias 'string-but-last '~string-but-last)
(defalias 'string-contains? '~string-contains?)
(defalias 'symbol->string '~symbol->string)
(defalias 'string->symbol '~string->symbol)
(defalias 'join-strings '~join-strings)
(defalias 'trim-spaces '~trim-spaces)
(defalias 'first-char-as-string '~first-char-as-string)
(defalias 'last-char-as-string '~last-char-as-string)

;; then helm-etags-select

(defun ~create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -R %s"
           (executable-find "ctags")
           (directory-file-name dir-name))))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo
find-tag.  If buffer is modified, ask about save before running
etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them
silently."
  (interactive)
  (shell-command (format "%s -e -R *.%s"
                         *ctags-path*
                         (or extension "*")
                         ))
  (let ((tags-revert-without-query t)) ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

(defun ~goto-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

(defalias 'goto-tag 'find-tag)
(defalias 'create-tags '~create-tags)
(defalias 'goto-tag-other-window '~goto-tag-other-window)

(defun ~surround (begin-string end-string)
  "Surround current selection with `begin-string` at the
beginning and `end-string` at the end.  If selection is not
active, insert `begin-string` and `end-string` and place the
cursor in-between them."
  (interactive "sStart string: \nsEnd string: ")
  (cond 
   ((is-selecting?)
    (save-excursion
      (let ((start-point (selection-start))
            (end-point   (selection-end)))
        (goto-point start-point)
        (insert begin-string)
        (goto-point end-point)
        (forward-char (length begin-string))
        (insert end-string))))

   (t
    (insert (concat begin-string end-string))
    (backward-char (length end-string)))))

(defun ~markdown-italicize ()
  "Italicize selection or adding italic format."
  (interactive)
  (~surround "*" "*"))

(defun ~markdown-embolden ()
  "Embolden selection or adding bold format."
  (interactive)
  (~surround "**" "**"))

(defun ~markdown-rawify ()
  "Rawify selection or adding raw format."
  (interactive)
  (~surround "`" "`"))

(defun ~move-to-beginning-of-line ()
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line."
  (interactive)

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line nil))))

(defun ~duplicate-line ()
  "Duplicate current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (yank)
  (newline)
  (yank)
  (beginning-of-line)
  (previous-line))

(defun ~open-line (arg)
  "Open line and move to the next line."
  (interactive "p")
  (end-of-line)
  (delete-horizontal-space)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun ~open-line-before (arg)
  "Open line and move to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun ~toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case, in that
cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
      (setq pos1 (region-beginning)
            pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (unless (eq last-command this-command)
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]")
          (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]")
          (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]")
          (put this-command 'state "init caps"))
         (t (put this-command 'state "all lower"))
         )))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2)
      (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2)
      (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2)
      (put this-command 'state "all lower")))))

(defun ~fix-hard-wrapped-region (begin end)
  "Fix hard-wrapped paragraphs."
  (interactive "r")
  (shell-command-on-region begin end "fmt -w 2500" nil t))

(defun ~mark-word ()
  "Put point at beginning of current word, set mark at end."
  (interactive)
  (let* ((opoint (point))
         (word (current-word))
         (word-length (length word)))
    (if (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (> (point-min)  (- (point) word-length))
            (beginning-of-buffer)
            (forward-char (- (length word))))
          (search-forward word (+ opoint (length word))
                          'noerror))
      (progn (push-mark (match-end 0) nil t)
             (goto-char (match-beginning 0)))
      (error "No word at point" word))))

(defun ~mark-line ()
  "Mark current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line))

(defun ~insert-me ()
  "Insert my information."
  (interactive)
  (insert *me*))

(defun ~delete-line ()
  "Delete current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(defun ~get-text (start end)
  "Return text from current buffer between start and end point."
  (if (or (< start (point-min))
          (< (point-max) end))
    ""
    (buffer-substring start end)))

(defun ~current-char ()
  "Return the string representing the character at the current
cursor position."
  (~get-text (point) (+ 1 (point))))

(defun ~peek-char ()
  "Peek next character, return the string representing it.."
  (~get-text (+ 1 (point)) (+ 2 (point))))

(defun ~join-with-next-line ()
  "Join next line with the current line.  This is just a
convenient wrapper of `join-line'."
  (interactive)
  (join-line -1))

(defun ~selection-start ()
  "Return the position of the start of the current selection."
  (region-beginning))

(defun ~selection-end ()
  "Return the position of the end of the current selection."
  (region-end))

(defun ~is-selecting? ()
  "Determine if a selection is being held."
  (region-active-p))

(defun ~current-selection ()
  "Return the current selected text."
  (if (is-selecting?)
    (buffer-substring (selection-start)
                      (selection-end))
    ""))

(defun ~get-selection ()
  "Return the current selected text."
  (~current-selection))

(defun ~delete-selected-text ()
  "Delete the selected text, do nothing if none text is selected."
  (if (~is-selecting?)
    (delete-region (selection-start) (selection-end))))

(defun ~replace-selection (&optional text)
  "Replace selection with text."
  (interactive)
  (let ((text (if (stringp text)
                text
                (read-string "Text: "))))
    (call-interactively 'delete-region)
    (insert-string text)))

(defun ~goto-selection-start ()
  "Go to the start of current selection.  If selection is not active,
do nothing."
  (if (~is-selecting?)
    (goto-point (selection-start))))

(defun ~goto-selection-end ()
  "Go to the end of current selection.  If selection is not active,
do nothing."
  (if (~is-selecting?)
    (goto-point (selection-end))))

(defun ~electrify-return-if-match (arg)
  "If the text after the cursor matches `electrify-return-match'
then open and indent an empty line between the cursor and the
text.  Move the cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at *electrify-return-match*)
        (save-excursion (newline-and-indent)))
    (newline arg)
    (indent-according-to-mode)))

(defun ~is-selecting? ()
  "Determine if a selection is being held."
  (region-active-p))

(defun ~emacs-lisp-make-alias ()
  "Add \(defalias 'symbol-1 'symbol-2\) to the end of file.
Useful when defining custom Emacs Lisp alias.  I use it when I
want to define a function with `~` prefix to prevent naming
clash, then add \(an\) alias\(es\) without `~`.

By default both `symbol-1` and `symbol-2` are the current
selection."
  (interactive)
  (when (~is-selecting?)
    (save-excursion
      (let* ((new-symbol (read-string "New symbol: " (~current-selection)))
             (old-symbol (read-string "Old symbol: " (~current-selection))))
        (~insert-text-at-the-end (format "(defalias '%s '%s)\n"
                                         new-symbol old-symbol))))))

(defun ~insert-text-at-the-end (&optional text)
  "Insert current selected text at the end of current buffer."
  (interactive)
  (let ((text (cond ((stringp text)
                     text)
                    ((~is-selecting?)
                     (~current-selection))
                    (t
                     (read-string "Text: ")))))
    (call-interactively 'end-of-buffer)
    (insert-string text)))

(defun ~paste-text-at-the-end ()
  "Insert current selected text at the end of current buffer."
  (interactive)
  (call-interactively 'kill-ring-save)
  (end-of-buffer)
  (call-interactively 'yank))

(defun ~mark-word-backward (times)
  "Mark word backward."
  (interactive "p")
  (if (~is-selecting?)
    (kill-region (selection-start) (selection-end))
    (progn (if (and (not (eq last-command this-command))
                    (not (eq last-command 'mark-sexp)))
             (set-mark (point)))
           (backward-word times))))

(defun ~get-unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g.,
 LEFT-ARROW or GREATER-THAN into an actual Unicode character
 code."
  (decode-char 'ucs (case name
                      (left-arrow 8592)
                      (up-arrow 8593)
                      (right-arrow 8594)
                      (down-arrow 8595)
                      (double-vertical-bar #X2551)
                      (equal #X003d)
                      (not-equal #X2260)
                      (identical #X2261)
                      (not-identical #X2262)
                      (less-than #X003c)
                      (greater-than #X003e)
                      (less-than-or-equal-to #X2264)
                      (greater-than-or-equal-to #X2265)
                      (logical-and #X2227)
                      (logical-or #X2228)
                      (logical-neg #X00AC)
                      ('nil #X2205)
                      (horizontal-ellipsis #X2026)
                      (double-exclamation #X203C)
                      (prime #X2032)
                      (double-prime #X2033)
                      (for-all #X2200)
                      (there-exists #X2203)
                      (element-of #X2208)
                      (square-root #X221A)
                      (squared #X00B2)
                      (cubed #X00B3)
                      (lambda #X03BB)
                      (alpha #X03B1)
                      (beta #X03B2)
                      (gamma #X03B3)
                      (delta #X03B4))))

(defun ~substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN
with the Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
  (font-lock-add-keywords
      nil `((,pattern
             (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                       ,(~get-unicode-symbol symbol)
                                       'decompose-region)
                       nil))))))

(defun ~substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (~substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))

(defalias 'insert-text-at-the-end '~insert-text-at-the-end)
(defalias 'emacs-lisp-make-alias '~emacs-lisp-make-alias)
(defalias 'electrify-return-if-match '~electrify-return-if-match)
(defalias 'goto-selection-end '~goto-selection-end)
(defalias 'goto-selection-start '~goto-selection-start)
(defalias 'replace-selection '~replace-selection)
(defalias 'delete-selected-text '~delete-selected-text)
(defalias 'get-selection '~get-selection)
(defalias 'current-selection '~current-selection)
(defalias 'is-selecting? '~is-selecting?)
(defalias 'selection-end '~selection-end)
(defalias 'selection-start '~selection-start)
(defalias 'join-with-next-line '~join-with-next-line)
(defalias 'peek-char '~peek-char)
(defalias 'current-char '~current-char)
(defalias 'get-text '~get-text)
(defalias 'insert-me '~insert-me)
(defalias '~add-me '~insert-me)
(defalias 'add-me '~insert-me)
(defalias 'mark-line '~mark-line)
(defalias 'mark-word '~mark-word)
(defalias 'fix-hard-wrapped-region '~fix-hard-wrapped-region)
(defalias 'toggle-letter-case '~toggle-letter-case)
(defalias 'open-line-before '~open-line-before)
(defalias 'duplicate-line '~duplicate-line)
(defalias 'move-to-beginning-of-line '~move-to-beginning-of-line)
(defalias 'markdown-italicize '~markdown-italicize)
(defalias 'markdown-embolden '~markdown-embolden)
(defalias 'markdown-rawify '~markdown-rawify)
(defalias 'surround '~surround)
(defalias 'paste-text-at-the-end '~paste-text-at-the-end)
(defalias 'mark-word-backward '~mark-word-backward)
(defalias 'substitute-patterns-with-unicode '~substitute-patterns-with-unicode)
(defalias 'substitute-pattern-with-unicode '~substitute-pattern-with-unicode)
(defalias 'get-unicode-symbol '~get-unicode-symbol)

(defalias '~smart-forward-exp 'forward-word
  "Forward word")

(defalias 'current-point 'point
  "Return current position of the keyboard cursor in the
buffer.")

(defalias '~current-word 'current-word
  "Return the current word as string.")

(defalias 'goto-point 'goto-char
  "Goto `point` in the buffer")

(defun ~google (keyword)
  "Google a keyword in Firefox."
  (interactive (list (~read-string "Keyword: "
                                   :initial-input (get-selection))))
  (~firefox
   (format "https://encrypted.google.com/search?q=%s" keyword)))

(defun ~firefox (url)
  "Open a URL in Firefox."
  (interactive
   (list (read-string "URL: " (cond
                               ((is-selecting?)
                                (get-selection))
                               ((thing-at-point-url-at-point)
                                (thing-at-point-url-at-point))
                               (t
                                "https://encrypted.google.com/")))))
  (~send-to-mozrepl (format "switchToTabHavingURI('%s', true)" url)))

(defun ~refresh-firefox ()
  "Refresh current tab of Firefox browser."
  (interactive)
  ;; This function can be used when editing HTML/CSS/Web resources, so the
  ;; timeout is there for the file to properly saved.
  (~send-to-mozrepl "setTimeout(BrowserReload, 300)"))

(defun ~start-mozrepl ()
  "Start MozRepl."
  (interactive)
  (inferior-moz-start-process))

(defun ~send-to-mozrepl (string)
  "Send a string to MozRepl."
  (interactive "MCommand: ")
  (~start-mozrepl)                      ; Make sure MozRepl is up and running
  (comint-send-string (inferior-moz-process)
                      string))

(defun ~auto-reload-firefox-after-save-hook ()
  "Auto reload Firefox when saving."
  (add-hook 'after-save-hook
            '(lambda ()
               (interactive)
               (comint-send-string (inferior-moz-process)
                                   "setTimout(BrowserReload(), '1000');"))
            ;; buffer-local
            'append 'local))

(defalias 'google '~google)
(defalias 'firefox '~firefox)
(defalias '~open-url-in-firefox '~firefox)
(defalias 'open-url-in-firefox '~firefox)
(defalias 'refresh-firefox '~refresh-firefox)
(defalias 'start-mozrepl '~start-mozrepl)
(defalias 'send-to-mozrepl '~send-to-mozrepl)
(defalias 'auto-reload-firefox-after-save-hook '~auto-reload-firefox-after-save-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-active-modes ()
  "Return the list of minor modes are enabled in the current
buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (ignore-errors
                           (if (and (symbolp mode) (symbol-value mode))
                               (add-to-list 'active-modes mode))))
          minor-mode-list)
    active-modes))

(defun ~is-minor-mode-active? (minor)
  "Determine if a minor mode is active in current buffer."
  (memq minor (~get-active-modes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xah Lee's open last buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xah-recently-closed-buffers
  nil
  "alist of recently closed buffers. Each element is (buffer
name, file path). The max number to track is controlled by the
variable `xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max
  40
  "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer if it's not sticky/dedicated.  Use
prefix arg (`C-u') to force closing if it is.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the
  buffer is not associated with a file.

• Make sure the buffer shown after closing is a user buffer.

• If the buffer is editing a source file in an org-mode file,
  prompt the user to save before closing.

• If the buffer is a file, add the path to the list
  `xah-recently-closed-buffers'.

• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.  Else it is a
user buffer."
  (interactive)

  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Buffer '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))

      (let (ξemacs-buff-p
            (ξorg-p (string-match "^*Org Src" (buffer-name))))

        (setq ξemacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

        (if (string= major-mode "minibuffer-inactive-mode")
            (minibuffer-keyboard-quit)  ; if the buffer is minibuffer
          (progn
            ;; offer to save buffers that are non-empty and modified, even for
            ;; non-file visiting buffer. (because kill-buffer does not offer to
            ;; save buffers that are not associated with files)
            (when (and (buffer-modified-p)
                       (not ξemacs-buff-p)
                       (not (string-equal major-mode "dired-mode"))
                       (if (equal (buffer-file-name) nil)
                           (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                         t))
              (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
                  (save-buffer)
                (set-buffer-modified-p nil)))
            (when (and (buffer-modified-p)
                       ξorg-p)
              (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
                  (org-edit-src-save)
                (set-buffer-modified-p nil)))

            ;; save to a list of closed buffer
            (when (not (equal buffer-file-name nil))
              (setq xah-recently-closed-buffers
                    (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
              (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
                (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

            ;; close
            (kill-buffer (current-buffer))

            ;; if emacs buffer, switch to a user buffer
            (when (string-match "^*" (buffer-name))
              (next-buffer)
              (let ((i 0))
                (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
                  (setq i (1+ i)) (next-buffer)))))))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop xah-recently-closed-buffers))))

(defun xah-open-recently-closed ()
  "Open recently closed file."
  (interactive)
  (find-file (ido-completing-read "Open: "
                                  (mapcar (lambda
                                            (f) (cdr f))
                                          xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let ((buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer buf)
    (mapc (lambda (f) (insert (cdr f) "\n"))
          xah-recently-closed-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ee:functions)
