;;
;; Copyright (C) 2014-2016 Ha-Duong Nguyen (@cmpitg)
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

;;;
;;; For programmer Drovak layout
;;;

;; <menu> key is really convenient, so first we get rid of its default

(bind-key "<menu>" 'nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ErgoErmacs-inspired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "s-K" '~delete-line)
(bind-key "s-l" 'goto-line)

(bind-key "s-c" 'previous-line)
(bind-key "s-t" 'next-line)
(bind-key "s-h" 'backward-char)
(bind-key "s-n" 'forward-char)

;; No longer used
;; (use-package grizzl-read
;;   :config (progn
;;             (bind-key "s-c" 'grizzl-set-selection+1 *grizzl-keymap*)
;;             (bind-key "s-t" 'grizzl-set-selection-1 *grizzl-keymap*)))

;; (bind-key "s-d" 'move-beginning-of-line)
(bind-key "s-d" '~move-to-beginning-of-line)
(bind-key "s-D" 'move-end-of-line)

(bind-key "M-s-c" '(lambda nil (interactive) (previous-line 5)))
(bind-key "M-s-t" '(lambda nil (interactive) (next-line 5)))
(bind-key "s-H" 'beginning-of-buffer)
(bind-key "s-N" 'end-of-buffer)

(bind-key "s-g" 'backward-word)
(bind-key "s-G" 'backward-sexp)
(bind-key "s-r" 'forward-word)
(bind-key "s-R" 'forward-sexp)

;;; Deleting

(bind-key "s-u" 'delete-char)
(bind-key "s-e" 'backward-delete-char)
(bind-key "s-p" 'kill-word)
(bind-key "s-." 'backward-kill-word)

(bind-key "s-x" '(lambda ()
                   (interactive)
                   (kill-line)
                   (delete-horizontal-space)))
(bind-key "s-X" '~delete-line)

;;; Selection

(bind-key "s-_" '~mark-line)
(bind-key "s-)" '~mark-word)
(bind-key "s-S-SPC" 'mark-defun)

;;; Other

(bind-key "s--" 'comment-or-uncomment-region)
(bind-key "s-/" 'create-tags)
(bind-key "s-?" 'visit-tags-table)
(bind-key "s-f" 'query-replace-regexp)
(bind-key "s-F" 'query-replace)
;; (bind-key "<menu> m" 'toggle-menu-bar-mode-from-frame)

(bind-key "s-'" 'undo)
(bind-key "s-\"" 'undo-tree-redo)

;; (bind-key "s-s" 'isearch-forward-regexp)
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "s-s" 'helm-occur)
;; (bind-key "s-s" '~swoop-bare)

;;; With other libraries

(use-package paredit
  :config (progn
            (bind-key "s-." 'paredit-backward-kill-word paredit-mode-map)
            (bind-key "s-p" 'paredit-forward-kill-word  paredit-mode-map)
            (bind-key "s-r" 'forward-word               paredit-mode-map)
            (bind-key "s-g" 'backward-word              paredit-mode-map)
            (bind-key "s-C" 'paredit-backward-up        paredit-mode-map)
            (bind-key "s-T" 'paredit-forward-up         paredit-mode-map)
            (bind-key "s-R" 'paredit-forward            paredit-mode-map)
            (bind-key "s-G" 'paredit-backward           paredit-mode-map)

            (bind-key "C-c l (" '~parenthesize-last-sexp paredit-mode-map)))

(use-package helm
  :config (progn
            (bind-key "s-t" 'helm-next-line      helm-map)
            (bind-key "s-c" 'helm-previous-line  helm-map)))

(use-package swoop
  :commands swoop
  :config (progn
            (bind-key "s-t" 'swoop-action-goto-line-next swoop-map)
            (bind-key "s-c" 'swoop-action-goto-line-prev swoop-map)))

;;; This file should be load after custom functions are all loaded

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "M-x" 'helm-M-x)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x M-x" 'execute-extended-command)

(bind-key "<f11>" 'helm-show-kill-ring)
;; (bind-key "<f7>" 'project-explorer)
(bind-key "<f7>" 'neotree-toggle)
(bind-key "<M-f11>" 'helm-all-mark-rings)
(bind-key "<S-f11>" 'history-goto-history)
(bind-key "<C-f12>" 'gist-list)
;; (bind-key "<C-f12>" 'projectile-commander)

;;
;; Buffer
;;

(bind-key "<f2>" 'save-buffer)
(bind-key "<f3>" 'helm-find-files)
(bind-key "<S-f3>" 'helm-projectile-find-file)
(bind-key "<C-f3>" '~helm-find-files-current-dir)
(bind-key "C-<f9>" '~move-to-compilation-buffer)
(bind-key "<C-f4>" 'xah-close-current-buffer)
(bind-key "<C-delete>" 'xah-close-current-buffer)
(bind-key "M-<f4>" 'recentf-open-files)
(bind-key "<menu> <menu>" 'other-window)

(bind-key "<C-f2>" 'history-add-history)
;; (bind-key "<C-f2>" '(lambda () (interactive) (history-add-history t)))
(bind-key "C-;" 'history-prev-history)
(bind-key "C-," 'history-next-history)

(bind-key "C-S-t" 'xah-open-recently-closed)

(bind-key "C-M-v" '(lambda ()
                     (interactive)
                     (scroll-other-window 5)))
(bind-key "C-M-S-v" '(lambda ()
                       (interactive)
                       (scroll-other-window -5)))

(bind-key "C-x C-b" 'bs-show)
(bind-key "s-B" '~switch-to-last-buffer)
;; (bind-key "<f8>" 'sr-speedbar-toggle)
(bind-key "<S-f8>" 'helm-bookmarks)

(bind-key "C-x C-n" '~new-buffer)

;; (bind-key "<C-next>" 'tabbar-ruler-forward)
;; (bind-key "<C-prior>" 'tabbar-ruler-backward)

;;
;; Text
;;

(bind-key "<S-delete>" 'delete-region)

(bind-key "C-o" '~open-line)
(bind-key "C-S-O" '~open-line-before)
(bind-key "C-=" 'align-regexp)

(bind-key "s-w" 'whitespace-cleanup)

(bind-key "s-&" 'join-with-next-line)

;; Multiple cursors
(bind-key "s-+" 'mc/edit-lines)
(bind-key "C-#" 'mc/mark-next-like-this)
(bind-key "C-!" 'mc/mark-previous-like-this)
(bind-key "C-c C-." 'mc/mark-all-like-this)

(bind-key "s-k" 'ace-jump-mode)

(bind-key "s-=" 'er/expand-region)
(bind-key "M-z" 'zap-up-to-char)

;; (bind-key "<M-f7>" '(lambda () "Next DEBUG" (interactive) (search-forward "DEBUG")))
;; (bind-key "<S-M-f7>" '(lambda () "Previous DEBUG" (interactive) (search-backward "DEBUG")))
;; (bind-key "<M-f5>" '(lambda () "Next FIXME" (interactive) (search-forward "FIXME")))
;; (bind-key "<S-M-f5>" '(lambda () "Previous FIXME" (interactive) (search-backward "FIXME")))

;;
;; Executing
;;

(bind-key "<f9>" 'compile)
(bind-key "s-a" '~exec|-select-output)
(bind-key "s-A" '~exec>)
;; (bind-key "s-a" '~exec<)
;; (bind-key "s-A" '~popup-shell-command)
(bind-key "s-[" 'emamux:send-region)

(bind-key "s-m" '~eval-then-replace-last-exp)
(bind-key "s-b" '~eval-then-replace)

;;
;; Window
;;

(bind-key "<pause>" '~toggle-sticky-window)
(bind-key "S-<f4>" '~delete-window)

(eval-after-load 'icicles-cmd1
  '(progn
     ;; S-f4 is always mapped to delete-window
     (global-set-key [remap icicle-kmacro] '~delete-window)))

(bind-key "<f4>" 'helm-multi-swoop-all)
(bind-key "C-7" 'split-window-vertically)
(bind-key "C-5" 'split-window-horizontally)
(bind-key "C-%" '~one-window)

(bind-key "<M-S-left>" 'windmove-left)
(bind-key "<M-S-right>" 'windmove-right)
(bind-key "<M-S-up>" 'windmove-up)
(bind-key "<M-S-down>" 'windmove-down)

;;
;; Misc
;;

(bind-key "C-M-_" 'redo)

;;
;; Mode
;;

(bind-key "C-<menu> C-f" 'auto-fill-mode)
(bind-key "C-<menu> C-<menu> C-<menu>" (lambda ()
                                         (interactive)
                                         (set-fill-column 78)))
;; (bind-key "C-<menu> C-(" 'autopair-mode)
(bind-key "<C-menu> C-(" 'smartparens-mode)
(bind-key "C-<menu> C-p" 'paredit-mode)
(bind-key "C-<menu> C-e" '~activate-evil-local-mode)
(bind-key "s-; s-," '~activate-evil-local-mode)

(bind-key "C-<menu> C-w" 'whitespace-mode)
;; (bind-key "<C-menu> C-a" 'auto-complete-mode)
(bind-key "<C-menu> <C-return>" 'markdown-mode)

(bind-key "s-z" '~open-current-file-as-admin)
(bind-key "s-v" 'package-list-packages)

;; iBus mode
;; (bind-key "C-M-S-SPC" '~toggle-ibus)
;; (bind-key "s-\\" 'ibus-mode)

;; (bind-key "M-x" 'execute-extended-command)
;; (bind-key "M-/" 'dabbrev-expand)

;; helm-do-ag: Helm'ing while typing to search right away
;; helm-ag: search first, then Helm'ing while typing to filter
;; (bind-key "<f10>" 'helm-do-grep-ag)
;; (bind-key "<f10>" 'helm-ag)
;; (bind-key "<f10>" 'helm-ag-project-root)
(bind-key "<f10>" 'helm-do-ag-project-root)
(bind-key "<C-f10>" 'helm-do-ag)
;; (bind-key "<C-f10>" 'ack)

(bind-key "s-@" '~duplicate-line-or-region)

(bind-key "<f8>" 'helm-mini)
(bind-key "M-/" 'hippie-expand)
(bind-key "M-C-/" 'helm-dabbrev)

(bind-key "<f12>" 'helm-M-x)

;;
;; Mode specific
;;

;; Common Lisp SLIME

(bind-key "C-\\" 'slime-selector)

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
            (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
            (define-key emacs-lisp-mode-map (kbd "<f1>") '(lambda ()
                                                            (interactive)
                                                            (apropos (current-word))))
            (define-key emacs-lisp-mode-map (kbd "<S-f1>") 'find-function)))

(bind-key "s-#"               '~add-bracket-and-eval)

;; Markdown

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "s-SPC i") '~markdown-italicize)
            (define-key markdown-mode-map (kbd "s-SPC b") '~markdown-embolden)
            (define-key markdown-mode-map (kbd "s-SPC r") '~markdown-rawify)))

(eval-after-load 'yasnippet
  '(progn
     (bind-key "C-c & C-n" 'create-snippet yas-minor-mode-map)))

;; Fix open-file command in Xiki

(eval-after-load 'el4r
  '(bind-key "C-o" '~open-line))

;;
;; Use with eletric RET
;;
;; If RET is pressed when the cursor is before a closing paren, the following
;; code will add an extra newline. The extra newlines are re-gathered by
;; paredit-close-round, which ParEdit binds to “)” by default.
;;

(bind-key "RET" 'electrify-return-if-match)

(provide 'ee:keybindings)
