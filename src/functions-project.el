;;
;; Copyright (C) 2017 Ha-Duong Nguyen (@cmpitg)
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

(defun ~projectile-find-files-at-dir (dir)
  "Activate `projectile-find-files', taking a specific directory
as project root."
  ;; Ignore the obsolete, we do need the powerful dynamic binding capability
  ;; of flet that neither cl-flet nor cl-letf provides
  (flet ((projectile-project-root () dir)
         (projectile-current-project-files
          ()
          (let (files)
            (setq files (-mapcat #'projectile-dir-files
                                 (projectile-get-project-directories)))
            (projectile-sort-files files))))
    (call-interactively 'projectile-find-file)))

(defun ~find-files-current-dir ()
  "Activates `projectile-find-files', taking current directory as
project root."
  (interactive)
  ;; Ignore the obsolete, we do need the powerful dynamic binding capability
  ;; of flet that neither cl-flet nor cl-letf provides
  (flet ((projectile-project-root () default-directory)
         (projectile-current-project-files
          ()
          (let (files)
            (setq files (-mapcat #'projectile-dir-files
                                 (projectile-get-project-directories)))
            (projectile-sort-files files))))
    (call-interactively 'projectile-find-file)))

(defun* ~neotree (&optional (dir "."))
  "Activate neotree and make the window sticky."
  (interactive)
  (neotree-dir dir)
  ;; (call-interactively 'neotree)
  ;; (call-interactively 'other-window)
  (set-window-dedicated-p (selected-window) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading project management functions")
(provide 'ee:functions-project)
