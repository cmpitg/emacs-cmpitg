(defvar *config-dir* (file-name-directory (or load-file-name
                                              (buffer-file-name))))


(defun ~get-config (&rest paths)
  "Returns path to a config file or directory."
  (apply 'concat *config-dir* paths))


(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))


(package-initialize)

;; Fetch the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (cond
   ((fboundp 'el-get-self-update)
    (el-get-self-update))

   (t
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (goto-char (point-max))
       (eval-print-last-sexp))))))

;; el-get runs in sync mode
(el-get 'sync)


(let ((local-package-dir (~get-config "local-packages/")))
  (dolist (package-dir (directory-files local-package-dir))
    (add-to-list 'load-path (~get-config local-package-dir package-dir))))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


(defvar *essential-elpa-packages*
  '(dash                                ; "Modern" list processing
    ht                                  ; The missing hashtable library
    s                                   ; "Modern" string processing
    f                                   ; "Modern" file APIs
    cl                                  ; Common Lisp subset in Emacs Lisp
    cl-lib                              ; Common Lisp library
    helm                                ; Smart completion framework
    thingatpt                           ; Getting thing at current pointg
    multiple-cursors                    ; Sublime-like multiple cursors
    expand-region                       ; Expand selection based-on semantic
                                        ; units
    eldoc                               ; Echo area function signature
    popwin                              ; Better popwin window management,
                                        ; dispose with Esc or C-g
    dired+                              ; Enhanced Dired
    tar-mode                            ; Supports for tar
    saveplace                           ; Save and restore current editing
                                        ; point
    color-theme
    smooth-scrolling                    ; Smoother scrolling
    flx-ido                             ; Better ido
    smartscan                           ; Jump between occurrences of a symbol
    smex                                ; Better M-x
    fiplr                               ; Find file with fuzzy matching
    wgrep-ack                           ; Edittable Ack
    browse-kill-ring                    ; Browsable kill ring
    tabbar-ruler                        ; Tabbar
    )
  "Essential ELPA packages that are vital to this config.")

(defvar *essential-el-get-packages*
  '(later-do                            ; Async eval
    multi-scratch                       ; Multiple scratch buffers
    moz-repl                            ; MozRepl
    whitespace                          ; Display trailing whitespace
    json-mode                           ; For en/decoding JSON
    )
  "Essential packages that cannot be installed with ELPA but
  el-get.")

(dolist (pkg *essential-elpa-packages*)
  (eval `(use-package ,pkg
           :ensure ,pkg)))

(dolist (pkg *essential-el-get-packages*)
  (el-get-install pkg)
  (eval `(use-package ,pkg)))
