;;  -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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

(use-package switch-buffer-functions
  :demand t
  :config
  (progn
    (defun auto-pos-mouse:advice/auto-pos-mouse (orig-fun &rest args)
      "Automatically position mouse in a sensible way.  Intended
to be used as an advice."
      (apply orig-fun args)
      (condition-case ex
          (~auto-pos-mouse-position)
        ('error (message "Error when calling advice to automatically position mouse in a sensible way" ex))))

    (defun auto-pos-mouse:auto-pos-mouse-in-find-file ()
      "Automatically position mouse in a sensible way.  Intended
to be used with `find-file-hook'."
      (condition-case ex (~auto-pos-mouse-position)
        ('error (message "Error when trying to automatically position mouse in a sensible way in find-file: %S" ex))))

    (defun auto-pos-mouse:enable-auto-pos-mouse ()
      "Enables the auto-pos-mouse-position feature."
      (interactive)
      (add-hook 'find-file-hook #'auto-pos-mouse:auto-pos-mouse-in-find-file)
      (advice-add 'switch-to-buffer :around #'auto-pos-mouse:advice/auto-pos-mouse)
      (advice-add 'other-window :around #'auto-pos-mouse:advice/auto-pos-mouse)
      (with-eval-after-load "magit"
        (advice-add 'magit-display-buffer :around #'auto-pos-mouse:advice/auto-pos-mouse))
      (advice-add 'select-frame-set-input-focus :around #'auto-pos-mouse:advice/auto-pos-mouse))

    (defun auto-pos-mouse:disable-auto-pos-mouse ()
      "Disables the auto-pos mouse position feature."
      (interactive)
      (remove-hook 'find-file-hook #'auto-pos-mouse:auto-pos-mouse-in-find-file)
      (advice-remove 'switch-to-buffer #'auto-pos-mouse:advice/auto-pos-mouse)
      (advice-remove 'other-window #'auto-pos-mouse:advice/auto-pos-mouse)
      (advice-remove 'magit-display-buffer #'auto-pos-mouse:advice/auto-pos-mouse)
      (advice-remove 'select-frame-set-input-focus #'auto-pos-mouse:advice/auto-pos-mouse))

    (auto-pos-mouse:enable-auto-pos-mouse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring auto-pos-mouse-position feature")

(provide 'rmacs:config-module-auto-pos-mouse-position)
