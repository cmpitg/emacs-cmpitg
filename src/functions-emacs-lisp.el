;;
;; Copyright (C) 2017-2018 Ha-Duong Nguyen (@cmpitg)
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

;; Workaround to include the removed seq-map-indexed from the seq library
(require 'seq)
(unless (fboundp 'seq-map-indexed)
  (defun seq-map-indexed (function sequence)
    "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of
the sequence, and its index within the sequence."
    (let ((index 0))
      (seq-map (lambda (elt)
                 (prog1
                     (funcall function elt index)
                   (setq index (1+ index))))
               sequence))))

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

(defun ~line-match? (regexp)
  "Determines of the current line matchs a regular expression."
  (s-matches? regexp (thing-at-point 'line)))

(defun ~eval-string (str)
  "Evals a string."
  (interactive "sString: ")
  (eval (first (read-from-string (concat "(progn " str ")")))))

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
    (kill-region (~selection-start) (~selection-end))
    (insert (format "%s" value))))

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

(defun ~eval-last-sexp-or-region ()
  "Evals region if active, or evals last sexpr"
  (interactive)
  (if (~is-selecting?)
      (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(defun ~eval-current-sexp ()
  "Evals the current enclosing sexp."
  (interactive)
  (let ((current-point (point)))
    (call-interactively 'er/mark-outside-pairs)
    (call-interactively 'eval-region)
    (goto-char current-point))
  (setq deactivate-mark t))

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

(defun* ~bind-key-with-prefix (key command &key
                                   (keymap global-map)
                                   (evil-keymap evil-normal-state-map))
  "Binds key in `evil-normal-state-map' with prefix `SPC' and in
global mode map with prefix `s-SPC' at the same time."
  (interactive)
  (eval `(progn
           (bind-key ,(format "s-SPC %s" key) command keymap)
           (bind-key ,(format "SPC %s" key) command evil-keymap))))

(defun* ~bind-key-with-prefix-local (key command &key (keymap global-map))
  "Like `~bind-key-with-prefix', except that instead of bindind
to `evil-normal-state-map' it binds to
`evil-normal-state-local-map'."
  (interactive)
  (~bind-key-with-prefix key command
                         :keymap keymap
                         :evil-keymap evil-normal-state-local-map))

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

(defun ~google (keyword)
  "Google a keyword in Firefox."
  (interactive (list (~read-string "Keyword: "
                                   :initial-input (get-selection))))
  (~firefox
   (format "https://encrypted.google.com/search?q=%s" keyword)))

(defun* ~firefox (url &key (new-window? nil))
  "Open a URL in Firefox."
  (interactive
   (list (read-string "URL: " (cond
                               ((is-selecting?)
                                (get-selection))
                               ((thing-at-point-url-at-point)
                                (thing-at-point-url-at-point))
                               (t
                                "https://encrypted.google.com/")))))
  ;; (~send-to-mozrepl (format "switchToTabHavingURI('%s', true)" url))
  (toolbox:run-process (if new-window?
                           (format "firefox-beta --new-window '%s'" url)
                         (format "firefox-beta '%s'" url))))

(defun ~konqueror (path)
  (interactive "MPath or URL: ")
  (toolbox:run-process (format "konqueror '%s'" path)))

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
  (interactive)
  (memq minor (~get-active-modes)))

(defun ~man-current-word ()
  "`man` this word."
  (interactive)
  (manual-entry (current-word)))

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

(defun ~copy-file-path ()
  "Copies file path to clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to clipboard." filename))))

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
  (if (not (window-minibuffer-p (selected-window)))
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

(defun ~copy-directory ()
  "Copy current directory to clipboard."
  (interactive)
  (~clipboard<- (~current-dir)))

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

(defun ~compile-haml ()
  "Compile HAML file to HTML file."
  (interactive)
  (and (string-match ".*\.haml$" (~current-file-full-path))
       (let ((output-file (replace-regexp-in-string "\.haml$" ".html"
                                                    (~current-file-full-path))))
         (compile (concat "haml \"" (~current-file-full-path) "\" "
                          "\"" output-file "\"")))))

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

(defun ~put-mode-line-to-top ()
  "Put the mode-line to the top of the window."
  (setq header-line-format mode-line-format mode-line-format nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AsciiDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~asciidoc/render (html-path)
  "Renders current file with AsciiDoctor in HTML format."
  (interactive "FHTML output path: ")
  (let ((cmd (format "asciidoctor --out-file '%s' '%s'"
                     html-path
                     (~current-file-full-path))))
    (toolbox:run-process cmd)
    (message "'%s' has finished running" cmd)
    (message "Check %s for output." html-path)))

(defun ~asciidoc/preview ()
  "Renders and previews current AsciiDoc file in HTML format with
Konqueror."
  (interactive)
  (let ((html-path (~asciidoc/current-temporary-html-path)))
    (~asciidoc/render html-path)
    ;; (~konqueror html-path)
    (~firefox html-path :new-window? t)))

(defun ~asciidoc/current-temporary-html-path ()
  "Returns the HTML path corresponding to the current AsciiDoc
buffer.  The path is stored in a buffer local variable named
`asciidoc-html-path' and generated if not yet exists"
  (let ((asciidoc-html-path/symbol (make-local-variable 'asciidoc-html-path)))
    (unless (boundp asciidoc-html-path/symbol)
      (set asciidoc-html-path/symbol (make-temp-file (f-filename (buffer-file-name))
                                                     nil
                                                     ".html")))
    asciidoc-html-path))

(defun ~asciidoc/update-preview ()
  "Re-renders current AsciiDoc file for preview.  The browser
needs manual refreshing."
  (interactive)
  (~asciidoc/render (~asciidoc/current-temporary-html-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GnuPG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun* ~update-gpg-agent-info (&optional (env-file "~/.gnupg/gpg-agent.env"))
  "Updates GnuPG agent info and returns a list of `cons'es that
described the updated list."
  (interactive)
  (let ((content (~read-file env-file)))
    (-map (lambda (var=val)
            (let* ((elements (s-split "=" var=val))
                   (var      (first elements))
                   (val      (second elements)))
              (setenv var val)
              (cons var val)))
          (butlast (s-split "\n" content)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package/library management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unicode symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias '~eval-selection 'eval-region)
(defalias 'erun '~execute-command-and-popup-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading Emacs Lisp helpers")
(provide 'ee:functions-emacs-lisp)
