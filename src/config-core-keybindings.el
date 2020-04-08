;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018-2020 Ha-Duong Nguyen (@cmpitg)
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

  ;; Deletion
  (bind-key "s-u" #'delete-char)
  (bind-key "s-e" #'backward-delete-char)
  (bind-key "s-p" #'kill-word)
  (bind-key "s-." #'backward-kill-word)
  (bind-key "<C-delete>" #'delete-region)
  (bind-key "x" #'delete-region evil-visual-state-map)
  (bind-key "M-z" #'evil-normal-state evil-insert-state-map)
  (bind-key "M-z" #'evil-normal-state evil-visual-state-map)
  (bind-key "M-z" #'evil-normal-state evil-normal-state-map)

  ;; Selection/region
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
  (bind-key "s-&" #'~join-with-next-line)
  (bind-key "s-b" #'pop-to-mark-command)

  (with-eval-after-load "smartparens"
    (bind-key "s-C" #'sp-backward-up)
    (bind-key "s-T" #'sp-up-sexp)
    (bind-key "M-s" #'sp-splice-sexp)
    (bind-key "M-S" #'sp-split-sexp))

  ;; Buffer management
  (bind-key "C-<tab>" #'iflipb-next-buffer)
  (bind-key "C-S-<tab>" #'iflipb-previous-buffer)
  (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer)
  (bind-key "C-S-t" #'~undo-killed-buffers)

  (bind-key "s-!" #'~execute-text-prompt)
  (bind-key "s-[" #'emamux:send-region)
  (bind-key "<s-RET>" #'~execute)
  (bind-key "<S-RET>" #'~execute-line)
  (bind-key "<s-return>" #'~execute)
  (bind-key "<S-return>" #'~execute-line)
  (bind-key "<C-down-mouse-1>" nil)
  (bind-key "<down-mouse-2>" nil)
  (bind-key "<mouse-2>" #'~execute)
  (bind-key "<mouse-3>" #'~popup-right-click-menu)
  (bind-key "<C-down-mouse-3>" #'~popup-right-click-menu)
  (bind-key "<f1>" #'~firefox)
  ;; Return in Evil executes commands based on context
  ;; (advice-add 'evil-ret :around #'~advice/evil-ret-execute)

  ;; Semantic editting
  (bind-key "s-\\" #'counsel-imenu)
  (~bind-key-evil "C-p" #'ivy-imenu-anywhere)

  (bind-key "<M-left>" #'windmove-left)
  (bind-key "<M-right>" #'windmove-right)
  (bind-key "<M-up>" #'windmove-up)
  (bind-key "<M-down>" #'windmove-down)

  ;; Header line
  (bind-key "<header-line> <mouse-3>" #'~header-line-execute)
  (bind-key "<header-line> <M-mouse-3>" #'~header-line-edit)

  ;;
  ;; Emacs Lisp
  ;;

  (bind-key "<C-return>"   #'~eval-last-sexp-or-region)
  (bind-key "<M-return>"   #'eval-defun)
  (bind-key "<C-S-return>" #'~eval-last-sexp-pp)
  (bind-key "s-m" #'~eval-then-replace-region-or-last-sexp)

  ;; (~bind-key-with-prefix "h f" #'describe-function)
  ;; (~bind-key-with-prefix "h v" #'describe-variable)
  ;; (~bind-key-with-prefix "h k" #'describe-key)
  ;; (~bind-key-with-prefix "h ." #'find-function)
  ;; (~bind-key-with-prefix "h l" #'find-library)

  ;;
  ;; Configuration & quick opening
  ;;

  ;;
  ;; Function keys & other convenient bindings
  ;;

  ;; TODO: Hydra
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
  (bind-key "<f4>" #'kill-this-buffer)
  ;; (bind-key "<f4>" #'~kill-buffer-and-frame)
  (bind-key "<C-f4>" #'~kill-buffer-and-window)
  (bind-key "<S-f4>" #'~delete-window)
  (bind-key "<f8>" #'~switch-buffer)
  (bind-key "<M-f8>" #'switch-to-buffer-other-frame)
  (bind-key "<S-f8>" #'counsel-bookmark)
  (bind-key "<f9>" #'compile)
  (bind-key "<f10>" #'counsel-ag)
  (bind-key "<M-f10>" #'ivy-resume)
  (bind-key "<S-f10>" #'~counsel-ag-default-project-root)
  (bind-key "<C-f10>" #'swiper-all)
  (bind-key "<f11>" #'counsel-yank-pop)
  (bind-key "<f12>" #'counsel-M-x)
  (bind-key "<S-f12>" #'~toggle-project-toolbox)

  (bind-key "M-/" #'hippie-expand)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (bind-key "M-ESC" #'~keyboard-quit)
  (bind-key "s-SPC s-SPC" #'exchange-point-and-mark)

  (use-package hydra
    :config
    (progn
      (defhydra hydra-visit (:exit t)
        "Visiting places"
        ("n" #'~open-project-notes "Current project notes")
        ("b" #'~open-project-toolbox "Current project toolbox")
        ("t" #'~open-toolbox "My global toolbox"))

      (defhydra hydra-file (:exit t)
        "File operations"
        ("d" #'~delete-current-file "Delete current file")
        ("r" #'~rename-current-file "Rename/move current file")
        ("a" #'~open-current-file-as-admin "Open current file as admin")
        ("c" #'~copy-file-name-to-clipboard "Copy path to clipboard")
        ("p" #'~copy-pos-to-clipboard "Copy current position to clipboard"))

      (defhydra hydra-jump (:exit t)
        "(Semantic) jumping"
        ("." #'dumb-jump-go "Try jumping to definition")
        ("," #'dumb-jump-back "Jump back")
        (";" #'dumb-jump-quick-look "Peek")
        ("s" #'point-pos-save "Save current point pos")
        ("n" #'point-pos-next "Next point pos" :exit nil)
        ("p" #'point-pos-prev "Previous point pos" :exit nil)
        ("g" #'point-pos-goto "Go to current point pos"))

      (defhydra hydra-cursor nil
        "Cursor operations"
        ("l" #'mc/edit-lines "Edit lines")
        ("n" #'mc/mark-next-like-this "Mark next like this")
        ("p" #'mc/mark-previous-like-this "Mark previous like this")
        ("a" #'mc/mark-all-in-region "Mark all in region")
        ("q" nil "Quit" :exit t))

      (defhydra hydra-buffer (:exit t)
        "Buffer operations"
        ("r" #'revert-buffer "Revert")
        ("n" #'~new-buffer "New")
        ("m" #'mark-whole-buffer "Mark whole")
        ("l" #'~show-buffer-chooser "Show buffer chooser")
        ("kk" #'kill-this-buffer "Kill current buffer")
        ("kb" #'kill-buffer "Kill a buffer"))

      (defhydra hydra-exec (:exit t)
        "Text execution"
        ("|" #'~exec| "Execute, piping region out and output to the current buffer")
        ("<" #'~exec< "Execute, piping output in")
        (">" #'~exec> "Execute, piping region out and output to a separate buffer")
        ("!" #'~execute-text-prompt "Prompt for text to be executed")
        ("l" #'~execute-line "Execute current line")
        ("e" #'~execute "Execute thing based on current context"))

      (defhydra hydra-emacs-lisp (:exit t)
        "Emacs Lisp operations"
        ("ee" #'~eval-last-sexp-or-region "Eval last sexp or region")
        ("ep" #'pp-eval-last-sexp "Eval and pretty-print last sexp")
        ("ew" #'~eval-then-replace-region-or-last-sexp "Eval and replace the last sexp with result")
        ("ex" #'eval-expression "Eval expression")
        ("hf" #'describe-function "Describe function")
        ("hv" #'describe-variable "Describe variable")
        ("hk" #'describe-key "Describe key binding")
        ("h." #'find-function "Jump to function definition")
        ("hl" #'find-library "Jump to library definition"))

      (defhydra hydra-insertion (:exit t)
        "Insertion operations"
        (";" #'~insert-full-line-comment "Insert line full of comment")
        ("e" #'~insert-exec "Insert executable"))

      (defhydra hydra-format (:exit t)
        "Formatting operations"
        ("ff" #'fill-paragraph "Format/fill paragraph")
        ("fp" #'paredit-reindent-defun "Reindent defun with Paredit")
        ("u" #'~unfill-paragraph "Unfill paragraph"))

      (defhydra hydra-mode (:exit t)
        "Mode operations"
        ("SPC" #'whitespace-mode "Toggle whitespace mode")
        ("gf" #'global-font-lock-mode "Toggle global font lock mode")
        ("lf" #'font-lock-mode "Toggle local font lock mode")
        ("w" #'~toggle-soft-wrapping "Toggle soft wrapping mode"))

      (defhydra hydra-window (:exit t)
        "Window management"
        ("sh" #'split-window-horizontally "Split window horizontally")
        ("sv" #'split-window-vertically "Split window vertically")
        ("kw" #'delete-window "Kill current window")
        ("kk" #'~kill-buffer-and-window "Kill current window with its buffer")
        ("T" #'~scroll-other-window "Scroll other window" :exit nil)
        ("C" #'~scroll-other-window-reverse "Scroll other window (reverse)" :exit nil)
        ("o" #'~one-window "One window (close others)")
        ("t" #'~transpose-windows "Transpose windows")
        ("y" #'~toggle-sticky-window "Toggle stickiness for current window")
        ("z" #'~toggle-maximize-buffer "Toggle max. for current window")
        ("r" #'resize-window "Interactive window resize")
        ("w" #'other-window "Interactive window resize" :exit nil)
        ("q" nil "Quit"))

      (defhydra hydra-frame (:exit t)
        "Frame management"
        ("n" #'make-frame "New frame")
        ("k" #'delete-frame "Delete current frame"))

      (defhydra hydra-edit (:exit t)
        "Editing operations"
        ("ss" #'~search-buffer-interactively "Interactive search")
        ("qrr" #'query-replace-regexp "Query replace regexp")
        ("qrq" #'query-replace "Query replace")
        ("aa" #'align "Align")
        ("ac" #'align-current "Align current section")
        ("ar" #'align-regexp "Align regexp")
        ("gs" #'magit-status "Magit status")
        ("c" #'comment-or-uncomment-region "Toggle commenting region")
        ("d" #'~duplicate-line-or-region "Duplicate current line or region")
        ("k" #'kill-sexp "Kill sexp"))

      (defhydra hydra-mark nil
        "Mark/region management"
        ("e" #'er/expand-region "Expand region")
        ("o" #'er/mark-outside-pairs "Mark outside pairs")
        ("f" #'er/mark-defun "Mark defun" :exit t)
        ("SPC" #'mark-sexp "Mark sexp")
        ("q" nil "Quit" :exit t))

      (defhydra hydra-global (:exit t)
        "Global operations"
        ("SPC" #'counsel-M-x "M-x")
        ("qq" #'save-buffers-kill-emacs "Save buffers and kill Emacs")
        ("d" #'hydra-edit/body "Editing")
        ("v" #'hydra-visit/body "Visiting")
        ("f" #'hydra-file/body "File")
        ("b" #'hydra-buffer/body "Buffer")
        ("c" #'hydra-cursor/body "Multiple cursors")
        ("j" #'hydra-jump/body "(Semantic) jumping")
        ("e" #'hydra-exec/body "Execution")
        ("l" #'hydra-emacs-lisp/body "Emacs Lisp operations")
        ("i" #'hydra-insertion/body "Insertion")
        ("t" #'hydra-format/body "Formatting")
        ("s" #'hydra-mode/body "Mode")
        ("m" #'hydra-mark/body "Marking")
        ("w" #'hydra-window/body "Window management")
        ("r" #'hydra-frame/body "Frame management")
        ("uj" #'hydra-clojure/body "Clojure development"))

      (bind-key "s-SPC" #'hydra-global/body)
      (evil-define-key 'normal global-map (kbd "SPC") #'hydra-global/body)
      (evil-define-key 'visual global-map (kbd "SPC") #'hydra-global/body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core keybindings")

(provide 'rmacs:config-core-keybindings)
