;;
;; Copyright (C) 2012-2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
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

;;; This file should be load after custom functions are all loaded

;; <menu> key is really convenient, so first we get rid of its default
;; use

(bind-key "<menu>" 'nil)

;;
;; ErgoEmacs bindings
;;

(bind-key "M-x" 'smex)
(bind-key "C-x M-x" 'execute-extended-command)

;;
;; Buffer
;;

(bind-key "<f2>" 'save-buffer)
(bind-key "C-<f2>" 'buffer-menu)
(bind-key "<f3>" 'fiplr-find-file)
(bind-key "<S-f3>" '~find-file-extended)
(bind-key "C-<f9>" '~move-to-compilation-buffer)
(bind-key "C-<f4>" '~kill-current-buffer)
(bind-key "M-<f4>" 'recentf-open-files)
(bind-key "C-<f1>" '~switch-to-scratch)
(bind-key "<menu> <menu>" 'other-window)

(bind-key "C-M-v" '(lambda ()
                                (interactive)
                                (scroll-other-window 6)))
(bind-key "C-M-S-v" '(lambda ()
                                (interactive)
                                (scroll-other-window -6)))
 
(bind-key "<C-delete>" '~kill-current-buffer)

(bind-key "C-x C-b" 'bs-show)
(bind-key "s-B" '~switch-to-last-buffer)
;; (bind-key "<f8>" 'sr-speedbar-toggle)
(bind-key "<S-f8>" 'helm-bookmarks)

(bind-key "C-z" 'popwin:keymap)

(bind-key "C-x C-n" 'multi-scratch-new)

(bind-key "<C-next>" 'tabbar-ruler-forward)
(bind-key "<C-prior>" 'tabbar-ruler-backward)

;;
;; Text
;;

(bind-key "C-<f5>" 'previous-error)
(bind-key "C-<f7>" 'next-error)
(bind-key "C-o" '~open-line)
(bind-key "C-S-O" '~open-line-before)
(bind-key "C-=" 'align-regexp)
(bind-key "C-<home>" '~jekyll-add-last-updated)

(bind-key "<C-backspace>" '~mark-word-backward)

;; (bind-key "<menu> C-<return>" 'complete-symbol)
(bind-key "s-<return>" 'pabbrev-expand-maybe)

;; (bind-key "<C-return>" 'complete-tag)
(bind-key "<M-return>" 'ac-fuzzy-complete)

(bind-key "s-w" 'whitespace-cleanup)

(bind-key "s-&" 'join-with-next-line)

(bind-key "s-SPC s" 'surround)

;; Multiple cursors
(bind-key "s-+" 'mc/edit-lines)
(bind-key "C-#" 'mc/mark-next-like-this)
(bind-key "C-!" 'mc/mark-previous-like-this)
(bind-key "C-c C-." 'mc/mark-all-like-this)

(bind-key "s-=" 'er/expand-region)

(bind-key "<M-f7>" '(lambda () "Next DEBUG" (interactive) (search-forward "DEBUG")))
(bind-key "<S-M-f7>" '(lambda () "Previous DEBUG" (interactive) (search-backward "DEBUG")))
(bind-key "<M-f5>" '(lambda () "Next FIXME" (interactive) (search-forward "FIXME")))
(bind-key "<S-M-f5>" '(lambda () "Previous FIXME" (interactive) (search-backward "FIXME")))

;;
;; Executing
;;

(bind-key "<f9>" 'compile)
(bind-key "C-<f12>" '~open-shell)
(bind-key "s-a" '~exec-in-other-window)
(bind-key "s-A" '~exec-then-pipe)
(bind-key "M-s-a" '~pipe-then-exec)
(bind-key "M-s-A" '~pipe-then-exec-in-other-window)
(bind-key "<S-down-mouse-1>" nil)
(bind-key "<S-mouse-1>" '~exec-then-pipe-selection)

(bind-key "<s-menu> <s-menu>" '~exec-in-other-window)
(bind-key "<s-menu> s-!" '~exec-then-pipe)
(bind-key "<s-menu> s-@" '~pipe-then-exec)
(bind-key "<s-menu> s-\\" '~pipe-then-exec-in-other-window)

(bind-key "s-m" '~eval-then-replace-last-exp)
(bind-key "s-b" '~eval-then-replace)

;;
;; Window
;;

(bind-key "S-<f4>" 'delete-window)
(eval-after-load 'icicles-cmd1
  '(progn
     ;; S-f4 is always mapped to delete-window
     (global-set-key [remap icicle-kmacro] 'delete-window)))

;; (bind-key "<f4>" 'find-file-other-window)
(bind-key "<f4>" '~helm-multi-all)
(bind-key "C-7" 'split-window-vertically)
(bind-key "C-5" 'split-window-horizontally)
(bind-key "C-%" 'delete-other-windows)

(bind-key "<M-S-left>" 'windmove-left)
(bind-key "<M-S-right>" 'windmove-right)
(bind-key "<M-S-up>" 'windmove-up)
(bind-key "<M-S-down>" 'windmove-down)

;;
;; Misc
;;

(bind-key "C-M-_" 'redo)
;; (bind-key "<f1>" '~man-this)
;; (bind-key "<mouse-2>" '~eval-selection)

;; (bind-key "<menu> M-t t" '(lambda ()
;;                                        (interactive)
;;                                        (set-frame-parameter nil
;;                                         'alpha 78)))

;;
;; File navigation
;;

(bind-key "s-; f n" '~goto-my-notes)

;;
;; Mode
;;

(bind-key "C-<menu> C-f" 'auto-fill-mode)
(bind-key "C-<menu> C-<menu> C-<menu>" (lambda ()
                                                     (interactive)
                                                     (set-fill-column 78)))
(bind-key "C-<menu> C-(" 'autopair-mode)
(bind-key "C-<menu> C-p" 'paredit-mode)
(bind-key "C-<menu> C-e" 'evil-mode)
(bind-key "C-<menu> C-w" 'whitespace-mode)
(bind-key "C-M-S-SPC" '~toggle-ibus)
(bind-key "<C-menu> C-a" 'auto-complete-mode)
(bind-key "<C-menu> <C-return>" 'markdown-mode)
(bind-key "s-z" '~open-current-file-as-admin)
(bind-key "s-v" 'package-list-packages)
(bind-key "s-\\" 'ibus-mode)
;; (bind-key "M-x" 'execute-extended-command)
;; (bind-key "M-/" 'dabbrev-expand)

(bind-key "<M-f3>" 'helm-find-files)
(bind-key "<f10>" '~helm-grep)
(bind-key "<C-f10>" 'ack)
(bind-key "s-@" '~duplicate-line)
(bind-key "<f8>" 'helm-buffers-list)
(bind-key "M-/" 'hippie-expand)
;; (bind-key "M-/" 'helm-dabbrev)
(bind-key "<f12>" 'helm-M-x)
;; (add-to-list 'helm-boring-buffer-regexp-list "\\*.+\\*")
(setq helm-boring-buffer-regexp-list '("\\*.+\\*"))
;; (setq helm-command-prefix-key "<f5>")

;;
;; Mode specific
;;

;; Git

(bind-key "s-SPC g" '~scm-status)

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
            (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
            (define-key emacs-lisp-mode-map (kbd "<f1>") '(lambda ()
                                                            (interactive)
                                                            (apropos (current-word))))
            (define-key emacs-lisp-mode-map (kbd "<S-f1>") 'find-function)
            ))
;; Markdown

(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "s-SPC i") '~markdown-italicize)
            (define-key markdown-mode-map (kbd "s-SPC b") '~markdown-embolden)
            (define-key markdown-mode-map (kbd "s-SPC r") '~markdown-rawify)))


(bind-key "<C-return>"        'wand:execute)
(bind-key "<C-S-return>"      'wand:execute-current-line)
(bind-key "<M-S-return>"      'toolbox:execute-and-replace)

(bind-key "<C-mouse-1>"       'wand:execute)
(bind-key "<C-down-mouse-1>"  nil)

(bind-key "<M-mouse-1>"       'toolbox:execute-and-replace)
(bind-key "<M-down-mouse-1>"  nil)

(bind-key "s-#"               '~add-bracket-and-eval)

(bind-key "s-SPC e" (lambda ()
                      "Find file in my emacs-config."
                      (interactive)
                      (~find-file-extended "~/emacs-config")))

(use-package grizzl-read
  :config (progn
            (bind-key "s-c" 'grizzl-set-selection+1 *grizzl-keymap*)
            (bind-key "s-t" 'grizzl-set-selection-1 *grizzl-keymap*)))

;;
;; Bookmark jumping
;;

(bind-key "C-h DEL" '(lambda ()
                       "Jump to keymap config"
                       (interactive)
                       (~open-file "~/emacs-config/config-default/keymap-common.el")))

(bind-key "C-h <return>" '(lambda ()
                            "Jump to ~/emacs-config/README.md"
                            (interactive)
                            (~open-file "~/emacs-config/README.md")))

(bind-key "s-SPC <return>"  '~open-url-in-firefox)
(bind-key "<s-return>"      'switch-to-eshell-back-and-forth)
(bind-key "<s-S-return>"    'cd-current-buffer-dir-and-switch-to-eshell)
(bind-key "<C-menu> C-("    'smartparens-mode)

(bind-key "<C-f3>"          'projectile-switch-project)
(bind-key "<f11>"           'helm-projectile)
(bind-key "<S-f9>"          'projectile-commander)

(eval-after-load 'markdown-mode
  '(progn
     (bind-key "s-SPC &" 'markdown-outline-headings markdown-mode-map)))

(eval-after-load 'emacs-lisp-mode
  '(progn
     (bind-key "s-SPC l d" 'emacs-lisp-make-function emacs-lisp-mode-map)))

(eval-after-load 'yasnippet
  '(progn
     (bind-key "C-c & C-n"       'create-snippet yas-minor-mode-map)))

;;
;; Inserting special symbols
;;

(bind-key "s-SPC s-SPC \\" (lambda () (interactive) (insert "λ")))
(bind-key "s-SPC s-SPC !"  (lambda () (interactive) (insert "¡")))
(bind-key "s-SPC s-SPC c"  (lambda () (interactive) (insert "©")))

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

;;
;; (Emacs) Lisp mode
;;

(bind-key "C-c l m a" '~emacs-lisp-make-alias emacs-lisp-mode-map)
