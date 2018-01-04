;; -*- no-byte-compile: t -*-

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

;; TODO: Bindings to spare
;; - s-x
;; - s-X

;; TODO: Functions:
;; - save place in buffers
;; - toolbox:open (from Rmacs invocation) aliased
;; - ~undo-kill-buffers
;; - ~gui/open-file
;; - ~gui/save-as
;; - ~switch-to-scratch
;; - ~new-buffer
;; - ~find-file-current-dir
;; - ~switch-to-last-buffer
;; - e b eval-buffer
;; - e e ~eval-current-expression
;; - e r eval-region
;; - e r (python-mode) python-shell-send-region
;; - g s for Magit
;; - ~join-with-next-line
;; - s-& ~join-with-next-line
;; - (bind-key "s-+" 'mc/edit-lines)

;; TODO: company mode
;; TODO: Make backward/forward kill word deletes, not kills

;; Doc: local/delete-on-exit
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

;; TODO: keybindings
;; er/mark-word
;; er/mark-symbol
;; er/mark-symbol-with-prefix
;; er/mark-next-accessor
;; er/mark-method-call
;; er/mark-inside-quotes
;; er/mark-outside-quotes
;; er/mark-inside-pairs
;; er/mark-outside-pairs
;; er/mark-comment
;; er/mark-url
;; er/mark-email
;; er/mark-defun

;; er/mark-html-attribute
;; er/mark-inner-tag
;; er/mark-outer-tag

;; TODO - Think about C-x C-c C-v should work as expected
;; TODO: Align mode
;; TODO: Diminish WK, acme, and ,
;; TODO: temp file
;; TODO: GUI open file
;; TODO: Current directory
;; TODO: Set current project directory of all visited files
;; TODO: Make point-pos open up closed file
;; TODO: Recent file list based on Emacs server name
;; TODO: toolbox:open
;; TODO: Migrate all old advice functions to the new advice mechanism as with :commands, so force-load might be necessary
;; TODO: Think about how testing should work

(defvar *bootstrap-config-path*
  (concat (file-name-directory (or (buffer-file-name)
                                   load-file-name))
          "bootstrap-config.el")
  "Path to the Rmacs boostrap-config file.")

(require 'rmacs:bootstrap-config        *bootstrap-config-path*)
(require 'rmacs:config-package-manager  "config-package-manager-rewrite")
(require 'rmacs:config-core-edit        "config-core-edit")
(require 'rmacs:config-core-behaviors   "config-core-behaviors")
(require 'rmacs:config-core-functions   "config-core-functions")
(require 'rmacs:config-core-keybindings "config-core-keybindings")
(require 'rmacs:config-ipc              "config-ipc-rewrite")

(message "Done loading Rmacs bare")
