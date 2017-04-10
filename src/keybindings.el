;;
;; Copyright (C) 2014-2017 Ha-Duong Nguyen (@cmpitg)
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

;; <menu> key is convenient, so we get rid of its default
(bind-key "<menu>" 'nil)

;; Remove this prefix key by any chance
(bind-key "s-SPC" 'nil)

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

(bind-key "s-x" '~kill-line-delete-spaces)
(bind-key "s-X" '~delete-line)

;;; Selection

(bind-key "s-_" '~mark-line)

;;; Other

(bind-key "s--" 'comment-or-uncomment-region)
(bind-key "s-f" 'query-replace-regexp)
(bind-key "s-F" 'query-replace)

(bind-key "s-'" 'undo-tree-undo)
(bind-key "s-\"" 'undo-tree-redo)

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "s-s" 'helm-occur)

;;; With other libraries

(with-eval-after-load "paredit"
  (bind-key "s-." 'paredit-backward-kill-word paredit-mode-map)
  (bind-key "s-p" 'paredit-forward-kill-word  paredit-mode-map)
  (bind-key "s-r" 'forward-word               paredit-mode-map)
  (bind-key "s-g" 'backward-word              paredit-mode-map)
  (bind-key "s-C" 'paredit-backward-up        paredit-mode-map)
  (bind-key "s-T" 'paredit-forward-up         paredit-mode-map)
  (bind-key "s-R" 'paredit-forward            paredit-mode-map)
  (bind-key "s-G" 'paredit-backward           paredit-mode-map)

  (bind-key "C-c l (" '~parenthesize-last-sexp paredit-mode-map))

(with-eval-after-load "helm"
  (bind-key "s-t" 'helm-next-line      helm-map)
  (bind-key "s-c" 'helm-previous-line  helm-map))

(with-eval-after-load "swoop"
  (bind-key "s-t" 'swoop-action-goto-line-next swoop-map)
  (bind-key "s-c" 'swoop-action-goto-line-prev swoop-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x M-x" 'execute-extended-command)

(bind-key "<f11>" 'helm-show-kill-ring)
;; (bind-key "<f7>" 'project-explorer)
;; (bind-key "<f7>" 'neotree-toggle)
(bind-key "<M-f11>" 'helm-all-mark-rings)
(bind-key "<S-f11>" 'history-goto-history)

;;
;; Buffer
;;

(bind-key "<f2>" 'save-buffer)
(bind-key "<f3>" 'helm-find-files)
(bind-key "<S-f3>" 'helm-projectile-find-file)
(bind-key "<C-f3>" '~helm-find-files-current-dir)
(bind-key "C-<f9>" '~move-to-compilation-buffer)
(bind-key "<C-tab>" '~switch-to-last-buffer)
(bind-key "s-\\" 'helm-semantic-or-imenu)

(bind-key "<C-f4>" 'xah-close-current-buffer)
(bind-key "<C-delete>" 'xah-close-current-buffer)
(bind-key "<menu> <menu>" 'other-window)
(bind-key "C-S-t" 'xah-open-recently-closed)

(bind-key "C-M-v" '~scroll-other-window)
(bind-key "C-M-S-v" '~scroll-other-window-reverse)

(bind-key "C-x C-b" 'bs-show)
(bind-key "s-B" '~switch-to-last-buffer)
(bind-key "<S-f8>" 'helm-bookmarks)
(bind-key "<f8>" 'helm-mini)

(bind-key "C-x C-n" '~new-buffer)
(with-eval-after-load "vdiff-mode"
  (bind-key "<f6>" vdiff-mode-prefix-map vdiff-mode-map))
(with-eval-after-load "evil-mode"
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map))

;;
;; Text
;;

(bind-key "<S-delete>" 'delete-region)
(bind-key "s-@" '~duplicate-line-or-region)

(bind-key "C-o" '~open-line)
(bind-key "C-S-O" '~open-line-before)
(bind-key "C-=" 'align-regexp)

(bind-key "s-w" 'whitespace-cleanup)

(bind-key "s-&" 'join-with-next-line)
(bind-key "s-; ; ;" '~insert-full-line-comment)

;; Multiple cursors

(bind-key "s-+" 'mc/edit-lines)
(bind-key "C-#" 'mc/mark-next-like-this)
(bind-key "C-!" 'mc/mark-previous-like-this)
(bind-key "C-c C-." 'mc/mark-all-like-this)

(bind-key "s-k" 'ace-jump-mode)
(bind-key "s-j" 'ace-jump-buffer)
(bind-key "s-J" 'ace-jump-buffer-other-window)
(bind-key "C-x SPC" 'ace-jump-mode-pop-mark)

(bind-key "s-=" 'er/expand-region)
(bind-key "M-z" 'zap-up-to-char)

;;
;; Executing
;;

(bind-key "<f9>" 'compile)
(bind-key "s-a" '~exec|)
(bind-key "s-A" '~exec>)
;; (bind-key "s-a" '~exec<)
;; (bind-key "s-A" '~popup-shell-command)
(bind-key "s-[" 'emamux:send-region)
(bind-key "<s-backspace>" 'srun)

(bind-key "<s-return>"      'switch-to-eshell-back-and-forth)
(bind-key "<s-S-return>"    'cd-current-buffer-dir-and-switch-to-eshell)

(bind-key "s-m" '~eval-then-replace-last-exp)
(bind-key "s-b" '~eval-then-replace)

(bind-key "s-#" 'eval-expression)


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

(bind-key "C-%" '~layout/default)
(bind-key "C-7" '~layout/vsplit)
(bind-key "C-5" '~layout/hsplit)

;; Undo & redo window layout
(~bind-key-with-prefix "w h" 'winner-undo)
(~bind-key-with-prefix "w n" 'winner-redo)

;;
;; Misc
;;

(bind-key "C-M-_" 'redo)
(bind-key "M-ESC" '~keyboard-quit)

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
(bind-key "<C-menu> <C-return>" 'markdown-mode)

(bind-key "s-z" '~open-current-file-as-admin)
(bind-key "s-v" 'package-list-packages)

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

(bind-key "M-/" 'hippie-expand)
(bind-key "M-C-/" 'helm-dabbrev)

(bind-key "<f12>" 'helm-M-x)

;;
;; Places
;;

(bind-key "<S-f12>" '~visit-toolbox)
(bind-key "C-<f1>" '~switch-to-scratch)
(bind-key "<S-f2>" '~switch-to-scratch-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Common Lisp SLIME

(add-hook 'slime-mode-hook
          (lambda ()
            (bind-key "C-\\" 'slime-selector)))

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
            (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
            (define-key emacs-lisp-mode-map (kbd "<f1>") '(lambda ()
                                                            (interactive)
                                                            (apropos (current-word))))
            (define-key emacs-lisp-mode-map (kbd "<S-f1>") 'find-function)))

;; Fix open-file command in Xiki

(with-eval-after-load "el4r"
  (bind-key "C-o" '~open-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; With evil-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "evil"
  ;; Movement
  (~bind-key-with-prefix "<" 'beginning-of-buffer)
  (~bind-key-with-prefix ">" 'end-of-buffer)
  (~bind-key-with-prefix "j" 'ace-jump-mode)
  (~bind-key-with-prefix "s-n" 'windmove-right)
  (~bind-key-with-prefix "s-h" 'windmove-left)
  (~bind-key-with-prefix "s-c" 'windmove-up)
  (~bind-key-with-prefix "s-t" 'windmove-down)
  (~bind-key-with-prefix "." 'dumb-jump-go)
  (~bind-key-with-prefix "," 'dumb-jump-back)
  (~bind-key-with-prefix ";" 'dumb-jump-quick-look)
  (~bind-key-with-prefix "\\" 'helm-semantic-or-imenu)

  ;; M-x
  (~bind-key-with-prefix "SPC" 'helm-M-x)
  (~bind-key-with-prefix "v p c" '~visit-experimental-config)
  (~bind-key-with-prefix "o p" '~open-project)
  (~bind-key-with-prefix "o f" '~open-file)

  ;; Buffer
  (~bind-key-with-prefix "s b" 'save-buffer)
  (~bind-key-with-prefix "l b" '~list-buffers)
  (~bind-key-with-prefix "r b" 'revert-buffer)
  (~bind-key-with-prefix "n n" '~new-buffer)
  (~bind-key-with-prefix "p p" 'popwin:messages)
  (~bind-key-with-prefix "z" '~toggle-maximize-buffer)

  ;; File
  (~bind-key-with-prefix "f o" 'ido-find-file-other-window)
  (~bind-key-with-prefix "f b" '~file/browse)
  (~bind-key-with-prefix "f c p" '~file/copy-path)
  (~bind-key-with-prefix "f c d" '~file/copy-directory)
  (~bind-key-with-prefix "o a" '~neotree)

  (~bind-key-with-prefix "m f n" '~my/file-notes)

  ;; Project
  (~bind-key-with-prefix "p s b" 'helm-projectile-switch-to-buffer)

  ;; Window
  (~bind-key-with-prefix "w c" '~window/change)
  (~bind-key-with-prefix "w w" 'ace-window)
  (~bind-key-with-prefix "o w" '~one-window)
  (~bind-key-with-prefix "w s v" '~window/split-vertically)
  (~bind-key-with-prefix "w s s" '~window/split-horizontally)

  ;; Git
  (~bind-key-with-prefix "g s" '~git/status)

  ;; Config
  (~bind-key-with-prefix "c a" '~visit-package-config)
  (~bind-key-with-prefix "c k" '~visit-keybindings)
  (~bind-key-with-prefix "c p" '~visit-experimental-config)

  ;; Toolbox
  (~bind-key-with-prefix "t b" '~toolbox)
  (~bind-key-with-prefix "c l f" '~clone-file)
  (~bind-key-with-prefix "n f " 'make-frame)

  ;; Help
  (~bind-key-with-prefix "h k" '~help-key)
  (~bind-key-with-prefix "l k b" '~list-keybindings)

  ;; Google
  (~bind-key-with-prefix "g o o" '~google)

  ;; With Return key
  (bind-key "RET n" 'cmpitg:visit-notes evil-normal-state-map)
  (bind-key "RET g" 'magit-status evil-normal-state-map)
  (bind-key "C-z" 'keyboard-quit evil-normal-state-map)
  (bind-key "C-z" 'keyboard-quit evil-visual-state-map)
  (bind-key "C-z" 'keyboard-quit evil-motion-state-map)
  
  (add-hook 'lisp-mode-hook
            (lambda ()
              (~bind-key-with-prefix-local "h h" 'hyperspec-lookup)))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (~bind-key-with-prefix-local "h f" '~emacs-lisp/help-function))))

(defalias '~my/file-notes 'cmpitg:visit-notes)

(defalias '~emacs-lisp/help-function 'describe-function)
(defalias '~help-key 'describe-key)
(defalias '~list-keybindings 'describe-personal-keybindings)

(defalias '~project/open 'helm-projectile-switch-project)
(defalias '~open-project 'helm-projectile-switch-project)
(defalias '~open-file    'find-file)

(defalias '~toolbox '~visit-toolbox)

(defalias '~window/change 'other-window)
(defalias '~window/split-vertically 'split-window-vertically)
(defalias '~window/split-horizontally 'split-window-horizontally)

(defalias '~visit-package-config '~visit-cmpitg-package-config)

(defalias '~git/status 'magit-status)
(defalias '~file/browse 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use with eletric RET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; If RET is pressed when the cursor is before a closing paren, the following
;; code will add an extra newline. The extra newlines are re-gathered by
;; paredit-close-round, which ParEdit binds to “)” by default.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "RET" '~electrify-return-if-match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading essential keybindings")
(provide 'ee:keybindings)
