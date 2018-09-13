;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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

(require 'package)

(setq elpamr-default-output-directory "/m/src/local-elpa-mirror")

(dolist (package-archive `(("local-elpa-mirror" . ,elpamr-default-output-directory)
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.milkbox.net/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("elpy" . "https://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives package-archive))

(package-initialize)

;; Fetch the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))

;;
;; el-get
;;
;; Ref: https://github.com/dimitri/el-get
;;

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get-bundle cmpitg/acme-mouse)
(el-get-bundle djcb/mu)
(el-get 'sync)

;;
;; use-package
;;
;; Ref: https://github.com/jwiegley/use-package
;;

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; To reduce load time
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(use-package diminish
  :demand t)
(use-package bind-key
  :demand t)

;;
;; Local packages
;;

(dolist (pkg (let ((local-packages-dir (~get-config "local-packages/")))
               (if (file-exists-p local-packages-dir)
                   (directory-files local-packages-dir)
                 '())))
  (add-to-list 'load-path (~get-config "local-packages/" pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring package manager")

(provide 'rmacs:config-package-manager)
