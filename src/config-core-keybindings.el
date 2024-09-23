;; -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018-2024 Ha-Duong Nguyen (@cmpitg)
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

;;
;; Notes on centralizing bindings vs. bindings per load:
;; * Memory fades, thus remembering bindings is the enemy of cognitive load
;; * To quickly browse through the bindings, one way is to centralize it
;; * For high-level operations that are not specific to a library, use defalias
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Principle:
;; - For Programmer Dvorak layout only
;; - When in doubt, leave it out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Doesn't need with-eval-after-load?

;; ;; Don't tabify after rectangle commands
;; (setq cua-auto-tabify-rectangles nil)

;;
;; Basic editing
;;

;; Selection/region
(with-eval-after-load "expand-region"
  (bind-key "s-=" #'er/expand-region))

;; Text processing
(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)
(with-eval-after-load "undo-tree"
  (bind-key "s-'" #'undo-tree-undo)
  (bind-key "s-\"" #'undo-tree-redo))

(with-eval-after-load "smartparens"
  (bind-key "s-C" #'sp-backward-up)
  (bind-key "s-T" #'sp-up-sexp)
  (bind-key "M-s" #'sp-splice-sexp)
  (bind-key "M-S" #'sp-split-sexp))

;; Buffer management
(with-eval-after-load "iflipb"
  (bind-key "C-<tab>" #'iflipb-next-buffer)
  (bind-key "C-S-<tab>" #'iflipb-previous-buffer)
  (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer))

;; RIGHT HERE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core keybindings")

(provide 'rmacs:config-core-keybindings)
