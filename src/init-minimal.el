;; -*- no-byte-compile: t; lexical-binding: t; -*-

;;
;; Copyright (C) 2018-2025 Ha-Duong Nguyen (@cmpitg)
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

;; Doc: Don't use setq to customize, use `custom-set-variables'
;; Doc: To add a file variable to the prop line: add-file-local-variable-prop-line
;; Doc: g f → find-file-at-point
;; Doc: internal-temp-output-buffer-show
;; Doc: Explain the implementation of *exec* functions
;; Doc: https://www.emacswiki.org/emacs/AlignCommands
;; Doc: one-buffer-per-window
;; Doc: http://oremacs.com/swiper/ → current word/line/symbol into minibuffer
;; Doc: local/delete-on-close
;; Doc: RMACS_NO_MACHINE_INIT
;; Doc: shell-quote-argument when calling shell arguments
;; Doc: make executable when looking at shebang
;; Doc: soft wrapping
;; Doc: temp file
;; Doc: file local variable
;; Doc: Bind key when loading package or in keybinding section?
;; Doc: use-package with :hook, :bind, ... will be loaded in a delayed manner
;; Doc: minor-mode-list & minor-mode-alist
;; Doc: exchange-point-and-mark after yanking to re-highlight region
;; Doc: C-S-t and the implementation of ~undo-killed-buffers
;; Doc: projectile file operations
;; Doc: treemacs
;; Doc: Splitting & one window

;; Always starts with bare config
(load (file-name-concat (file-name-directory (or load-file-name
                                                 (buffer-file-name)))
                        "init-bare"))

(require 'rmacs:config-package-manager                "config-package-manager")
(require 'rmacs:config-core-functions                 "config-core-functions")
(require 'rmacs:config-module-org-mode                "config-module-org-mode")
(require 'rmacs:config-core-edit                      "config-core-edit")
(require 'rmacs:config-core-ux                        "config-core-ux")
(require 'rmacs:config-themes                         "config-themes")
(require 'rmacs:config-core-keybindings               "config-core-keybindings")
(require 'rmacs:config-module-convenient-buffer-shell "config-module-convenient-buffer-shell")

;;
;; Last but not least - remember
;;

(require 'rmacs:config-core-last                      "config-core-last")

;; Make user all actions queued by Elpaca are executed before we enjoy Emacs
(with-eval-after-load "elpaca"
  (elpaca-process-queues))

(message "Done loading Rmacs minimal")
