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

;; Install all essential packages

(dolist (package *essential-packages*)
  ;; They're all available with Elpa or Melpa
  (unless (package-installed-p package)
    (package-install package)))

;; Docs:
;;   https://github.com/magnars/dash.el
(use-package dash
  :init (progn
          (dash-enable-font-lock)))

(use-package helm-config)

(eval-after-load "helm-regexp"
  '(helm-attrset 'follow 1 helm-source-moccur))

;; Don't auto change-dir
(setq-default helm-ff-auto-update-initial-value nil)

;; Smex for enhancing M-x

(use-package smex
  :init (progn
          (smex-initialize)))

;; http://www.emacswiki.org/emacs/SmoothScrolling

(use-package smooth-scrolling)
