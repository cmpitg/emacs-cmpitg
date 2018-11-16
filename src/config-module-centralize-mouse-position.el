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
    (defun centralize-mouse:advice/centralize-mouse (orig-fun &rest args)
      "Centralize mouse position.  Intended to be used as an advice."
      (apply orig-fun args)
      (condition-case ex
          (progn
            (~centralize-mouse-position)
            (select-window (window-at (cadr (mouse-position))
                                      (cddr (mouse-position)))))
        ('error (message "Error when calling advice to centralize mouse position" ex))))

    (defun centralize-mouse:centralize-mouse-in-find-file ()
      "Centralize mouse position.  Intended to be used with
`find-file-hook'."
      (condition-case ex (~centralize-mouse-position)
        ('error (message "Error when trying to centralize mouse position in find-file: %S" ex))))

    (defun centralize-mouse:enable-centralizing-mouse ()
      "Enables the centralizing mouse position feature."
      (interactive)
      (add-hook 'find-file-hook #'centralize-mouse:centralize-mouse-in-find-file)
      (advice-add 'switch-to-buffer :around #'centralize-mouse:advice/centralize-mouse)
      (advice-add 'other-window :around #'centralize-mouse:advice/centralize-mouse)
      (with-eval-after-load "magit"
        (advice-add 'magit-display-buffer :around #'centralize-mouse:advice/centralize-mouse))
      (advice-add 'select-frame-set-input-focus :around #'centralize-mouse:advice/centralize-mouse))

    (defun centralize-mouse:disable-centralizing-mouse ()
      "Disables the centralizing mouse position feature."
      (interactive)
      (remove-hook 'find-file-hook #'centralize-mouse:centralize-mouse-in-find-file)
      (advice-remove 'switch-to-buffer #'centralize-mouse:advice/centralize-mouse)
      (advice-remove 'other-window #'centralize-mouse:advice/centralize-mouse)
      (advice-remove 'magit-display-buffer #'centralize-mouse:advice/centralize-mouse)
      (advice-remove 'select-frame-set-input-focus #'centralize-mouse:advice/centralize-mouse))

    (centralize-mouse:enable-centralizing-mouse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring centralize-mouse-position feature")

(provide 'rmacs:config-module-centralize-mouse-position)
