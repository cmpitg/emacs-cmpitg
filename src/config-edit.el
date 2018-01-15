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
;; En/decoding JSON
;;
;; Ref: https://github.com/joshwnj/json-mode
;;

(use-package json-mode
  :mode "\\.json\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring Rmacs for code & text editting")

(provide 'rmacs:config-edit)
