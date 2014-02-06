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

;; (eval-after-load 'js3-mode)
;; (setq js3-auto-indent-p t
;;       js3-enter-indents-newline t
;;       js3-indent-on-enter-key t)

(use-package js2-mode
  :init (progn
          (add-hook 'js-mode-hook 'js2-minor-mode)
          ($auto-load-mode '("\\.js\\'") 'js2-mode)
          (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

          (add-hook 'html-mode-hook '$auto-reload-firefox-after-save-hook)
          (add-hook 'css-mode-hook '$auto-reload-firefox-after-save-hook)))

