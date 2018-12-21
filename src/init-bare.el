;; -*- no-byte-compile: t; lexical-binding: t; -*-

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

;; TODO: rmacs --debug-on-error
;; TODO: tmux integration with emamux
;; TODO: See how https://github.com/joaotavora/sly could replace SLIME
;; TODO: https://github.com/tjarvstrand/edts and Erlang integration
;; TODO: https://wiki.haskell.org/Emacs#Scion for Haskell integration

;; TODO: DOC - Python mode keybinding - Look under the Elpy configuration

;; TODO: Manual testing scenario

;; TODO: *exec* in combination with the header-line
;; TODO: header-line → double-right-clicking to edit

;; TODO: Functions:
;; - toolbox:open (from Rmacs invocation) aliased
;; - e b eval-buffer
;; - e e ~eval-current-expression
;; - e r eval-region
;; - e r (python-mode) python-shell-send-region
;; - (bind-key "s-+" 'mc/edit-lines)

;; TODO: company mode
;; TODO: Make backward/forward kill word deletes, not kills

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

;; TODO: Make point-pos open up closed file
;; TODO: Migrate all old advice functions to the new advice mechanism as with :commands, so force-load might be necessary
;; TODO: Think about how testing should work
;; TODO: config-literate-programming's TODOs

(defvar *bootstrap-config-path*
  (concat (file-name-directory (or (buffer-file-name)
                                   load-file-name))
          "bootstrap-config.el")
  "Path to the Rmacs boostrap-config file.")

(require 'rmacs:bootstrap-config                        *bootstrap-config-path*)
(require 'rmacs:config-package-manager                  "config-package-manager")
(require 'rmacs:config-ipc                              "config-ipc")
(require 'rmacs:config-core-functions                   "config-core-functions")
(require 'rmacs:config-core-edit                        "config-core-edit")
(require 'rmacs:config-core-ux                          "config-core-ux")
(require 'rmacs:config-core-keybindings                 "config-core-keybindings")
(require 'rmacs:config-module-mru-buffer                "config-module-mru-buffer")
(require 'rmacs:config-module-auto-pos-mouse-position   "config-module-auto-pos-mouse-position")

(message "Done loading Rmacs bare")
