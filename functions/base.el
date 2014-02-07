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

(defun ~load-all-custom-functions ()
  "Load all custom functions in `*config-dir*/functions/`."
  (let* ((this-dir (~get-local-config-dir "functions/")))
    (dolist (file (directory-files this-dir))
      (let ((path (concat this-dir file)))
        (when
            (and (not (string-match "base\\.el$" path)) ; Not this
                                                                    ; file
                 (not (string-match "^[#$]" (file-name-base path))) ; Not
                                                                    ; backup
                                                                    ; file
                 (string-match "\\.el$" path))
          (load-file path))))))

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
