;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2019 Ha-Duong Nguyen (@cmpitg)
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

;; Open last session/visited files from the last session
;; Ref: https://github.com/nflath/save-visited-files
;; TODO: Problematic - requires scrolling with some files
(use-package save-visited-files
  :config (progn
            ;; Temporarily set default Scheme implementation to prevent
            ;; interactive prompt on startup
            (setq geiser-default-implementation 'guile)

            ;; Each Emacs server has a different list of visited files
            (setq save-visited-files-location
                  (format "~/.emacs.d/emacs-visited-files.%s" server-name))

            (unless (file-exists-p save-visited-files-location)
              (write-region "" nil save-visited-files-location))

            (turn-on-save-visited-files-mode)))

;; Managing recent files
(use-package recentf
  :init (progn
          (recentf-mode 1)
          (custom-set-variables `(recentf-max-menu-items 128)
                                `(recentf-save-file ,(format "~/.emacs.d/recentf.%s" server-name)))))

(message "Reverting all file-backed buffers")
(~revert-all-file-buffers-no-confirmation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core components (load last)")

(provide 'rmacs:config-core-last)
