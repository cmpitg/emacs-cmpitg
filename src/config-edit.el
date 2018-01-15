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

;;
;; YAML
;;
;; Ref: https://www.emacswiki.org/emacs/YamlMode
;;

(use-package yaml-mode)

;;
;; En/decoding JSON
;;
;; Ref: https://github.com/joshwnj/json-mode
;;

(use-package json-mode
  :mode "\\.json\\'")

;;
;; Vimdiff implementation - More intuitive than Ediff
;;
;; https://github.com/justbur/emacs-vdiff
;;

(use-package vdiff
  :commands vdiff-files)

;;
;; Markdown
;;
;; Ref: https://jblevins.org/projects/markdown-mode/
;;

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (progn
    (custom-set-faces
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(markdown-header-face-1
       ((t (:inherit markdown-header-face :height 1.7 :background "#ABCDEF"))))
     '(markdown-header-face-2
       ((t (:inherit markdown-header-face :height 1.5 :background "green"))))
     '(markdown-header-face-3
       ((t (:inherit markdown-header-face :height 1.3)))))))

;;
;; GPG interface
;;
;; Ref: https://www.emacswiki.org/emacs/EasyPG
;;
;; Visit anything.gpg and it will encrypt it when you save the buffer.
;;
;; To prevent EPG from prompting for a key every time you save a file, put the
;; following at the top of your file:
;;
;;    -*- epa-file-encrypt-to: ("your@email.address") -*-
;;

(use-package epa-file
  :config (epa-file-enable))

;;
;; Enhanced file management with Dired
;;

(use-package dired+
  :init (progn
          (setq dired-listing-switches "-lahF")
          ;; Reuse current buffer when opening file/dir
          (toggle-diredp-find-file-reuse-dir 1)))

(use-package dired-single)

(use-package dired-details+
  :after (dired-single)
  :config
  (setq dired-listing-switches "-lhFgG --group-directories-first"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring Rmacs for code & text editting")

(provide 'rmacs:config-edit)
