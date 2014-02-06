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

(use-package auto-complete
  :init (progn
          (require 'auto-complete-config)
          (ac-config-default)
          (setq ac-sources
                '(ac-source-filename
                  ac-source-functions
                  ;; ac-source-yasnippet
                  ac-source-variables
                  ac-source-symbols
                  ac-source-features
                  ac-source-abbrev
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary))

          (auto-complete-mode 1)
          (setq ac-fuzzy-enable t)

          (add-hook 'ruby-mode-hook
                    (lambda ()
                      (add-to-list 'ac-sources 'ac-source-rsense-method)
                      (add-to-list 'ac-sources 'ac-source-rsense-constant)))))
