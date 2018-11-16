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
    (defun centralize-mouse:advice/centralize-mouse (orig-fun buffer-or-name &rest args)
      "Centralize mouse position.  Intended to be used with `switch-to-buffer'"
      (condition-case ex (~centralize-mouse-position)
        ('error (message "Error when calling advice to centralize mouse position" ex)))
      (apply orig-fun buffer-or-name args))

    (defun centralize-mouse:centralize-mouse-with-prev-buffer (prev-buffer current-buffer)
      "Centralize mouse position.  Intended to be used with the
`switch-buffer-functions' hook."
      (condition-case ex (~centralize-mouse-position)
        ('error (message "Error when trying to centralize mouse position: %S.  Previous buffer: %S.  Current buffer: %S."
                         ex
                         prev-buffer
                         current-buffer))))

    (defun centralize-mouse:centralize-mouse-in-find-file ()
      "Centralize mouse position.  Intended to be used with
`find-file-hook'."
      (condition-case ex (~centralize-mouse-position)
        ('error (message "Error when trying to centralize mouse position in find-file: %S" ex))))

    (defun centralize-mouse:enable-centralizing-mouse ()
      "Enables the centralizing mouse position feature."
      (interactive)
      (add-hook 'switch-buffer-functions #'centralize-mouse:centralize-mouse-with-prev-buffer)
      (add-hook 'find-file-hook #'centralize-mouse:centralize-mouse-in-find-file)
      (advice-add 'switch-to-buffer :around #'centralize-mouse:advice/centralize-mouse))

    (defun centralize-mouse:disable-centralizing-mouse ()
      "Disables the centralizing mouse position feature."
      (interactive)
      (remove-hook 'switch-buffer-functions #'centralize-mouse:centralize-mouse-with-prev-buffer)
      (remove-hook 'find-file-hook #'centralize-mouse:centralize-mouse-in-find-file)
      (advice-remove 'switch-to-buffer #'centralize-mouse:advice/centralize-mouse))

    (centralize-mouse:enable-centralizing-mouse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring centralize-mouse-position feature")

(provide 'rmacs:config-module-centralize-mouse-position)
