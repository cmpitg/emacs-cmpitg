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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Principle:
;; - For Programmer Dvorak layout only
;; - Set only what's needed and remove the rest
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "evil"
  ;; (cua-mode t)
  ;; (setq cua-keep-region-after-copy t)
  ;; ;; Don't tabify after rectangle commands
  ;; (setq cua-auto-tabify-rectangles nil)
  ;; ;; No region when it is not highlighted
  ;; (transient-mark-mode 1)

  ;; <menu> key is convenient, so we get rid of its default
  (bind-key "<menu>" 'nil)
  ;; Remove this prefix key by any chance
  (bind-key "s-SPC" 'nil)

  ;;
  ;; Basic editing
  ;;

  ;; Movements
  (bind-key "s-c" 'previous-line)
  (bind-key "s-t" 'next-line)
  (bind-key "s-h" 'backward-char)
  (bind-key "s-n" 'forward-char)
  (bind-key "s-d" '~move-to-beginning-of-line)
  (bind-key "s-D" 'move-end-of-line)
  (bind-key "M-s-c" '~previous-line+)
  (bind-key "M-s-t" '~next-line+)
  (bind-key "s-H" 'beginning-of-buffer)
  (bind-key "s-N" 'end-of-buffer)
  (bind-key "s-g" 'backward-word)
  (bind-key "s-G" 'backward-sexp)
  (bind-key "s-r" 'forward-word)
  (bind-key "s-R" 'forward-sexp)
  (bind-key "s-l" 'goto-line)
  (bind-key "C-," 'point-pos-next)
  (bind-key "C-;" 'point-pos-previous)

  ;; Deletion
  (bind-key "s-u" 'delete-char)
  (bind-key "s-e" 'backward-delete-char)
  (bind-key "s-p" 'kill-word)
  (bind-key "s-." 'backward-kill-word)
  (bind-key "<C-delete>" 'delete-region)

  ;; Text processing
  (bind-key "s--" 'comment-or-uncomment-region)
  (bind-key "s-f" 'query-replace-regexp)
  (bind-key "s-F" 'query-replace)
  (bind-key "s-'" 'undo-tree-undo)
  (bind-key "s-\"" 'undo-tree-redo)
  (bind-key "C-s" 'isearch-forward-regexp)
  (bind-key "C-r" 'isearch-backward-regexp)
  (bind-key "s-s" 'swiper)
  (bind-key "s-_" '~mark-line)
  (bind-key "s-w" 'whitespace-cleanup)
  (bind-key "s-=" 'er/expand-region)
  (bind-key "s-@" '~duplicate-line-or-region)
  (bind-key "s-; ; ;" '~insert-full-line-comment)

  ;; Buffer management
  (bind-key "<C-tab>" '~switch-to-last-buffer)
  (bind-key "C-S-t" '~undo-killed-buffers)

  ;; Semantic editting
  (bind-key "s-\\" 'counsel-imenu)
  (~bind-key-evil "C-p" 'ivy-imenu-anywhere)

  ;; Window
  (bind-key "C-%" '~one-window)
  (bind-key "C-7" 'split-window-vertically)
  (bind-key "C-5" 'split-window-horizontally)
  (~bind-key-with-prefix "o w" '~one-window)

  ;; Marking
  (~bind-key-with-prefix "m e" 'er/mark-outside-pairs)
  (~bind-key-with-prefix "m f" 'er/mark-defun)

  ;;
  ;; Emacs Lisp
  ;;

  (bind-key "<C-return>" '~eval-last-sexp-or-region emacs-lisp-mode-map)
  (bind-key "<M-return>" 'eval-defun emacs-lisp-mode-map)
  (bind-key "s-#" 'eval-expression)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (~bind-key-with-prefix-local "h f" 'describe-function)
              (~bind-key-with-prefix-local "h v" 'describe-variable)
              (~bind-key-with-prefix-local "h k" 'describe-key)
              (~bind-key-with-prefix-local "h ." 'find-function)
              (~bind-key-with-prefix-local "h l" 'find-library)))

  ;;
  ;; Function keys & other convenient bindings
  ;;

  (bind-key "<C-f1>" '~switch-to-scratch)
  (bind-key "<f2>" 'save-buffer)
  (bind-key "<S-f2>" '~gui/save-as)
  (bind-key "<C-f2>" 'point-pos-save)
  (bind-key "<f3>" 'counsel-find-file)
  (bind-key "<C-f3>" '~find-files-current-dir)
  (bind-key "<M-f3>" '~gui/open-file)
  (bind-key "<S-f3>" 'projectile-find-file)
  (bind-key "<C-f4>" 'kill-this-buffer)
  (bind-key "<S-f4>" '~delete-window)
  (bind-key "<f8>" 'ivy-switch-buffer)
  (bind-key "<S-f8>" 'counsel-bookmark)
  (bind-key "<f11>" 'counsel-yank-pop)
  (bind-key "<f12>" 'counsel-M-x)

  (bind-key "s-SPC s-SPC" 'exchange-point-and-mark)

  (~bind-key-with-prefix "q q" 'save-buffers-kill-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core keybindings")

(provide 'rmacs:config-core-keybindings)
