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
