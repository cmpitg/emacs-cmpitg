;; -*- lexical-binding: t; -*-

;;
;; Copyright (C) 2018-2019 Ha-Duong Nguyen (@cmpitg)
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
;; - When in doubt, leave it out
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
  (bind-key "s-c" #'previous-line)
  (bind-key "s-t" #'next-line)
  (bind-key "s-h" #'backward-char)
  (bind-key "s-n" #'forward-char)
  (bind-key "s-d" #'~move-to-beginning-of-line)
  (bind-key "s-D" #'move-end-of-line)
  (bind-key "M-s-c" #'~previous-line+)
  (bind-key "M-s-t" #'~next-line+)
  (bind-key "s-H" #'beginning-of-buffer)
  (bind-key "s-N" #'end-of-buffer)
  (bind-key "s-g" #'backward-word)
  (bind-key "s-G" #'backward-sexp)
  (bind-key "s-r" #'forward-word)
  (bind-key "s-R" #'forward-sexp)
  (bind-key "s-l" #'goto-line)
  (bind-key "C-," #'point-pos-next)
  (bind-key "C-;" #'point-pos-previous)
  (bind-key "<M-f2>" #'point-pos-goto)

  ;; Deletion
  (bind-key "s-u" #'delete-char)
  (bind-key "s-e" #'backward-delete-char)
  (bind-key "s-p" #'kill-word)
  (bind-key "s-." #'backward-kill-word)
  (bind-key "<C-delete>" #'delete-region)
  (bind-key "x" #'delete-region evil-visual-state-map)

  ;; Selection/region
  (bind-key "s-_" #'~mark-line)
  (bind-key "s-=" #'er/expand-region)

  ;; Searching
  (bind-key "C-s" #'isearch-forward-regexp)
  (bind-key "C-r" #'isearch-backward-regexp)
  (bind-key "s-s" #'~search-buffer-interactively)
  (bind-key "s-f" #'query-replace-regexp)
  (bind-key "s-F" #'query-replace)

  ;; Text processing
  (bind-key "RET" #'~electrify-return-if-match)
  (bind-key "s--" #'comment-or-uncomment-region)
  (bind-key "s-'" #'undo-tree-undo)
  (bind-key "s-\"" #'undo-tree-redo)
  (bind-key "s-w" #'whitespace-cleanup)
  (bind-key "s-@" #'~duplicate-line-or-region)
  (bind-key "C-o" #'~open-line)
  (bind-key "C-S-o" #'~open-line-before)
  (bind-key "C-=" #'align-regexp)
  (bind-key "s-&" #'~join-with-next-line)
  (bind-key "M-Q" #'~unfill-paragraph)
  (bind-key "s-; ; ;" #'~insert-full-line-comment)
  (bind-key "s-b" #'pop-to-mark-command)

  (with-eval-after-load "smartparens"
    (bind-key "s-C" #'sp-backward-up)
    (bind-key "s-T" #'sp-up-sexp)
    (bind-key "M-s" #'sp-splice-sexp)
    (bind-key "M-S" #'sp-split-sexp))

  (~bind-key-with-prefix "c s" #'embrace-commander)
  (~bind-key-with-prefix "." #'dumb-jump-go)
  (~bind-key-with-prefix "," #'dumb-jump-back)
  (~bind-key-with-prefix ";" #'dumb-jump-quick-look)

  ;; Multiple cursors
  (bind-key "s-+" #'mc/edit-lines)
  (bind-key "C-#" #'mc/mark-next-like-this)
  (bind-key "C-!" #'mc/mark-previous-like-this)

  ;; File management
  (~bind-key-with-prefix "f d" #'~delete-current-file)
  (~bind-key-with-prefix "f r" #'~rename-current-file)
  (bind-key "s-z" #'~open-current-file-as-admin)

  ;; Project management
  (~bind-key-with-prefix "p p" #'treemacs)
  (~bind-key-with-prefix "p o" #'treemacs-projectile)
  (~bind-key-with-prefix "p f" #'treemacs-select-window)

  ;; Buffer management
  (bind-key "C-<tab>" #'iflipb-next-buffer)
  (bind-key "C-S-<tab>" #'iflipb-previous-buffer)
  (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer)
  (bind-key "C-S-t" #'~undo-killed-buffers)
  (~bind-key-with-prefix "b r" #'revert-buffer)
  (~bind-key-with-prefix "b n" #'~new-buffer)
  (~bind-key-with-prefix "b s" #'~new-buffer-frame-from-selection)
  (~bind-key-with-prefix "b m" #'mark-whole-buffer)
  (~bind-key-with-prefix "b l" #'~show-buffer-chooser)
  (~bind-key-with-prefix "b p a" #'~append-pos-to-window-on-the-right)

  ;; Execution
  (bind-key "s-a" #'~exec|)
  (bind-key "s-A" #'~exec<)
  (bind-key "M-a" #'~exec>)
  (bind-key "s-[" #'emamux:send-region)
  (bind-key "<s-return>" #'~execute)
  (bind-key "<C-down-mouse-1>" nil)
  ;; (bind-key "<C-mouse-1>" #'~execute)
  (bind-key "<down-mouse-2>" nil)
  (bind-key "<mouse-2>" #'~execute)
  (bind-key "<mouse-3>" #'~popup-right-click-menu)
  (bind-key "<C-down-mouse-3>" #'~popup-right-click-menu)
  (bind-key "<f1>" #'~firefox)
  ;; Return in Evil executes commands based on context
  (advice-add 'evil-ret :around #'~advice/evil-ret-execute)

  ;; Semantic editting
  (bind-key "s-\\" #'counsel-imenu)
  (~bind-key-evil "C-p" #'ivy-imenu-anywhere)

  ;; Window management
  (~bind-key-with-prefix "z" #'~toggle-maximize-buffer)
  (bind-key "C-%" #'~one-window)
  (bind-key "C-7" #'split-window-vertically)
  (bind-key "C-5" #'split-window-horizontally)
  (bind-key "<pause>" #'~toggle-sticky-window)

  (bind-key "C-M-v" #'~scroll-other-window)
  (bind-key "C-M-S-v" #'~scroll-other-window-reverse)

  (~bind-key-with-prefix "n f " #'make-frame)
  (~bind-key-with-prefix "w w" #'other-window)
  (~bind-key-with-prefix "w c" #'ace-window)
  (~bind-key-with-prefix "w s" #'ace-swap-window)
  (~bind-key-with-prefix "w o" #'~one-window)
  (~bind-key-with-prefix "w t" #'~transpose-windows)

  (~bind-key-with-prefix "r" #'resize-window)

  ;; Header line
  (bind-key "<header-line> <mouse-3>" #'~header-line-execute)
  (bind-key "<header-line> <M-mouse-3>" #'~header-line-edit)

  ;; Version management
  (~bind-key-with-prefix "g s" #'magit-status)

  ;; Display
  (~bind-key-with-prefix "s l" #'linum-mode)
  (~bind-key-with-prefix "s w" #'~toggle-soft-wrapping)
  (~bind-key-with-prefix "s f" #'global-font-lock-mode)
  (~bind-key-with-prefix "s SPC" #'whitespace-mode)

  ;; Marking
  (~bind-key-with-prefix "m e" #'er/mark-outside-pairs)
  (~bind-key-with-prefix "m f" #'er/mark-defun)

  ;;
  ;; Emacs Lisp
  ;;

  (bind-key "<C-return>"   #'~eval-last-sexp-or-region)
  (bind-key "<M-return>"   #'eval-defun)
  (bind-key "<S-return>"   #'~eval-current-sexp)
  (bind-key "<C-S-return>" #'~eval-last-sexp-pp)
  (bind-key "s-m" #'~eval-then-replace-region-or-last-sexp)
  (bind-key "s-#" #'eval-expression)
  (bind-key "s-!" #'~execute-text-prompt)

  (~bind-key-with-prefix "h f" #'describe-function)
  (~bind-key-with-prefix "h v" #'describe-variable)
  (~bind-key-with-prefix "h k" #'describe-key)
  (~bind-key-with-prefix "h ." #'find-function)
  (~bind-key-with-prefix "h l" #'find-library)

  ;;
  ;; Configuration & quick opening
  ;;

  (~bind-key-with-prefix "v i" #'~visit-init-bare)
  (~bind-key-with-prefix "v t" #'~visit-toolbox)
  (~bind-key-with-prefix "v b" #'~visit-project-toolbox)

  ;;
  ;; Function keys & other convenient bindings
  ;;

  (bind-key "<C-f1>" #'~toggle-scratch)
  (bind-key "<S-f1>" #'~switch-to-messages-buffer)
  (bind-key "<f2>" #'save-buffer)
  (bind-key "<S-f2>" #'~gui/save-as)
  (bind-key "<C-f2>" #'point-pos-save)
  (bind-key "<C-S-f2>" #'point-pos-delete)
  (bind-key "<f3>" #'counsel-find-file)
  (bind-key "<C-f3>" #'~find-files-current-dir)
  (bind-key "<M-f3>" #'~gui/open-file)
  (bind-key "<S-f3>" #'projectile-find-file)
  (bind-key "<f4>" #'swiper-all)
  ;; (bind-key "<C-f4>" #'kill-this-buffer)
  (bind-key "<C-f4>" #'~kill-buffer-and-window)
  (bind-key "<C-S-f4>" #'~kill-buffer-and-frame)
  (bind-key "<S-f4>" #'~delete-window)
  (bind-key "<f8>" #'~switch-buffer)
  (bind-key "<M-f8>" #'switch-to-buffer-other-frame)
  (bind-key "<S-f8>" #'counsel-bookmark)
  (bind-key "<f9>" #'compile)
  (bind-key "<f10>" #'counsel-ag)
  (bind-key "<M-f10>" #'ivy-resume)
  (bind-key "<S-f10>" #'~counsel-ag-default-project-root)
  (bind-key "<f11>" #'counsel-yank-pop)
  (bind-key "<f12>" #'counsel-M-x)
  (bind-key "<S-f12>" #'~toggle-project-toolbox)

  (bind-key "M-/" #'hippie-expand)
  (bind-key "M-ESC" #'~keyboard-quit)
  (bind-key "s-SPC s-SPC" #'exchange-point-and-mark)
  (~bind-key-with-prefix "SPC" #'counsel-M-x)
  (~bind-key-with-prefix "q q" #'save-buffers-kill-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core keybindings")

(provide 'rmacs:config-core-keybindings)
