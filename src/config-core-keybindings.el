;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018-2021 Ha-Duong Nguyen (@cmpitg)
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

;; TODO: Doesn't need with-eval-after-load?
(with-eval-after-load "modalka"
  ;; ;; Don't tabify after rectangle commands
  ;; (setq cua-auto-tabify-rectangles nil)

  ;;
  ;; Basic editing
  ;;

  ;; Selection/region
  (bind-key "s-=" #'er/expand-region)

  ;; Searching
  (bind-key "M-r" #'~insert-entry-from-exec-history)

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
  (bind-key "C-<tab>" #'iflipb-next-buffer)
  (bind-key "C-S-<tab>" #'iflipb-previous-buffer)
  (bind-key "<C-S-iso-lefttab>" #'iflipb-previous-buffer)
  (bind-key "C-S-t" #'~undo-killed-buffers)

  (bind-key "<s-RET>" #'~execute)
  (bind-key "<S-RET>" #'~execute-line)
  (bind-key "<M-S-RET>" #'(lambda ()
                            (interactive)
                            (call-interactively #'~execute-line)
                            (call-interactively #'kill-current-buffer)))
  (bind-key "<s-return>" #'~execute)
  (bind-key "<S-return>" #'~execute-line)
  (bind-key "<M-S-return>" #'(lambda ()
                               (interactive)
                               (call-interactively #'~execute-line)
                               (call-interactively #'kill-current-buffer)))
  (bind-key "<C-down-mouse-1>" nil)
  (bind-key "<down-mouse-2>" nil)
  (bind-key "<mouse-2>" #'~execute)
  (bind-key "<mouse-3>" #'~popup-context-menu)
  (bind-key "<C-down-mouse-3>" #'~popup-context-menu)
  (bind-key "<f1>" #'~firefox)

  ;; Semantic editting
  (bind-key "s-\\" #'counsel-imenu)

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

  ;; TODO: Keybinding for eval upper sexp
  (bind-key "<C-S-return>" #'~eval-last-sexp-pp)
  (bind-key "s-m"          #'~eval-then-replace-region-or-last-sexp)

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

  (bind-key "<C-f1>" #'~toggle-scratch)
  (bind-key "<S-f1>" #'~switch-to-messages-buffer)
  (bind-key "<S-f2>" #'~gui/save-as)
  (bind-key "<C-f2>" #'point-pos-save)
  (bind-key "<C-S-f2>" #'point-pos-delete)
  (bind-key "<f3>" #'counsel-find-file)
  (bind-key "<C-f3>" #'~find-files-current-dir)
  (bind-key "<M-f3>" #'~gui/open-file)
  (bind-key "<S-f3>" #'projectile-find-file)
  (bind-key "<S-f4>" #'~delete-window)
  (bind-key "<f8>" #'~switch-buffer)
  (bind-key "<M-f8>" #'switch-to-buffer-other-frame)
  (bind-key "<S-f8>" #'counsel-bookmark)
  (bind-key "<f9>" #'compile)
  (bind-key "<f10>" #'~counsel-rg)
  (bind-key "<M-f10>" #'ivy-resume)
  (bind-key "<S-f10>" #'~counsel-grep-default-project-root)
  (bind-key "<C-f10>" #'swiper-all)
  (bind-key "<f11>" #'counsel-yank-pop)
  (bind-key "<f12>" #'counsel-M-x)
  (bind-key "<S-f12>" #'~toggle-project-toolbox)

  (bind-key "M-/" #'hippie-expand)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (bind-key "M-ESC" #'~keyboard-quit)

  (use-package hydra
    :config
    (progn
      (defhydra hydra-visit (:columns 4 :exit t)
        "Visiting places"
        ("n" #'~open-project-notes "Current project notes")
        ("b" #'~open-project-toolbox "Current project toolbox")
        ("t" #'~open-toolbox "My global toolbox")
        ("pp" #'projectile-switch-project "Switch to project"))

      (defhydra hydra-file (:columns 4 :exit t)
        "File operations"
        ("d" #'~delete-current-file "Delete current file")
        ("r" #'~rename-current-file "Rename/move current file")
        ("s" #'save-buffer "Save file")
        ("a" #'~save-buffer-as "Save file as")
        ("o" #'counsel-find-file "Open file")
        ("f" #'projectile-find-file "Open file in project")
        ("e" #'~find-files-current-dir "Open file in current dir")
        ("i" #'~find-files-current-dir-not-ignoring "Open file in current dir, no ignore")
        ("m" #'~open-current-file-as-admin "Open current file as admin")
        ("c" #'~copy-file-name-to-clipboard "Copy path to clipboard")
        ("p" #'~copy-pos-to-clipboard "Copy current position to clipboard")
        ("t" #'~choose-path-and-act "Choose path and act"))

      (defhydra hydra-jump (:columns 4 :exit t)
        "(Semantic) jumping"
        ("a" #'ace-jump-mode "Jump to a char")
        ("l" #'ace-jump-line-mode "Jump to line")

        ("'" #'dumb-jump-go-other-window "Try jumping to def (other window)")
        ;; ("." #'dumb-jump-go "Try jumping to def")
        ;; ("," #'dumb-jump-back "Jump back")
        (";" #'dumb-jump-quick-look "Peek")
        ("." #'smart-jump-go "Try jumping to def")
        ("," #'smart-jump-back "Jump back")

        ("s" #'point-pos-save "Save current point pos")
        ("n" #'point-pos-next "Next point pos" :exit nil)
        ("p" #'point-pos-previous "Previous point pos" :exit nil)
        ("g" #'point-pos-goto "Go to current point pos")
        ("i" #'counsel-imenu "Symbol-based")
        ("j" #'~goto-next-line-matching-marker "Go to next line matching marker" :exit nil)
        ("k" #'~goto-prev-line-matching-marker "Go to prev line matching marker" :exit nil)

        ("ESC" nil "Quit" :exit t))

      (defhydra hydra-cursor (:columns 4)
        "Cursor operations"
        ("l" #'mc/edit-lines "Edit lines")
        ("n" #'mc/mark-next-like-this "Mark next like this")
        ("p" #'mc/mark-previous-like-this "Mark previous like this")
        ("a" #'mc/mark-all-in-region "Mark all in region")
        ("ESC" nil "Quit" :exit t))

      (defhydra hydra-buffer (:columns 4 :exit t)
        "Buffer operations"
        ("r" #'revert-buffer "Revert")
        ("n" #'~new-buffer "New")
        ("m" #'mark-whole-buffer "Mark whole")
        ("l" #'~show-buffer-chooser "Show buffer chooser")
        ("b" #'list-buffers "Show buffer list management")
        ("kk" #'kill-current-buffer "Kill current buffer")
        ("kb" #'kill-buffer "Kill a buffer"))

      (defhydra hydra-freeform-exec (:columns 2 :exit t)
        "Pattern-based text"
        ("i" #'~palette/point/exec-sh-in-term-mux "SH in term mux")
        ("I" #'~palette/point/exec-sh-in-term-mux-piping-to-sh-output-file "SH in term mux, piping output to file")
        ("k" #'~palette/point/exec-sh-piping-here "SH, piping output here")
        ("x" #'~palette/point/exec-sh-in-term-mux-then-pause "SH in term mux, then pause")
        ("a" #'~ansi-colorize-current-output-block "Colorize output block"))

      (defhydra hydra-exec (:columns 3 :exit t)
        "Text execution"
        ("|" #'~exec-sh| "Run sh command, piping region out and output in")
        ("<" #'~exec-sh< "Run sh command, piping output in")
        (">" #'~exec-sh> "Run sh command, piping region out and popping up output")
        ("p" #'~exec-sh-pop-up "Run sh command, popping up output")
        ("x" #'~execute-text-prompt "Prompt execution")
        ("l" #'~execute-line "Execute line")
        ("w" #'~execute-current-wand-text "Execute current Wand text")
        ("b" #'~exec-current-block "Execute current block")
        ("L" #'(lambda ()
                 (interactive)
                 (call-interactively #'~execute-line)
                 (call-interactively #'kill-current-buffer))
         "Execute line & kill buffer")
        ("e" #'~execute "Execute (context-based)")
        ("E" #'(lambda ()
                 (interactive)
                 (call-interactively #'~execute)
                 (call-interactively #'kill-current-buffer))
         "Execute (context-based) & kill buffer")
        ("j" #'~goto-prev-command-pattern "Go to prev command pattern" :exit nil)
        ("k" #'~goto-next-command-pattern "Go to next command pattern" :exit nil)
        ("r" #'~insert-entry-from-exec-history "Insert from history")
        ;; TODO: buffer shell current dir
        ;; ("b" #')
        ("s" #'(lambda ()
                 (interactive)
                 (~execute "!! zsh")) "Zsh"))

      (defhydra hydra-emacs-lisp (:columns 4 :exit t)
        "Emacs Lisp operations"
        ("ee" #'~eval-last-sexp-or-region "Eval last sexp/region")
        ("ep" #'pp-eval-last-sexp "Eval and pp last sexp")
        ("ew" #'~eval-then-replace-region-or-last-sexp "Eval and replace last sexp")
        ("ex" #'eval-expression "Eval expression")
        ("er" #'eval-region "Eval region")
        ("eb" #'eval-buffer "Eval buffer")

        ("hf" #'describe-function "Describe function")
        ("hv" #'describe-variable "Describe variable")
        ("hk" #'describe-key "Describe key binding")
        ("." #'find-function "Jump to function definition")
        ("l" #'find-library "Jump to library definition"))

      (defhydra hydra-insertion (:columns 4 :exit t)
        "Insertion operations"
        (";" #'~insert-full-line-comment "Insert line full of comment")
        ("<" #'(lambda () (interactive) (insert *~output-beginning-marker*)) "Output marker (start)")
        (">" #'(lambda () (interactive) (insert *~output-end-marker*)) "Output marker (end)")
        ("b" #'(lambda () (interactive) (insert *~output-beginning-marker* "\n" *~output-end-marker*) (previous-line)) "Output markers")
        ("x" #'~insert-entry-from-exec-history "Insert exec from history")
        ("e" #'~insert-exec "Insert executable"))

      (defhydra hydra-format (:columns 4 :exit t)
        "Formatting operations"
        ("ff" #'fill-paragraph "Format/fill paragraph")
        ("fp" #'paredit-reindent-defun "Reindent defun with Paredit")

        ("u" #'~unfill-paragraph "Unfill paragraph"))

      (defhydra hydra-mode (:columns 4 :exit t)
        "Mode operations"
        ("SPC" #'whitespace-mode "Toggle whitespace mode")
        ("gf" #'global-font-lock-mode "Toggle global font lock mode")
        ("lf" #'font-lock-mode "Toggle local font lock mode")
        ("w" #'~toggle-soft-wrapping "Toggle soft wrapping mode"))

      (defhydra hydra-window (:columns 4 :exit t)
        "Window management"
        ("sr" #'(lambda () (interactive) (~split-window 'right)) "Split right")
        ("sb" #'(lambda () (interactive) (~split-window 'below)) "Split below")
        ("k" #'delete-window "Kill")
        ;; ("kk" #'~kill-buffer-and-window "Kill current window & buffer")
        ("T" #'~scroll-other-window "Scroll other" :exit nil)
        ("C" #'~scroll-other-window-reverse "Scroll other (reverse)" :exit nil)
        ("o" #'~one-window "One window (close others)")
        ("t" #'~transpose-windows "Transpose windows")
        ("y" #'~toggle-sticky-window "Toggle stickiness")
        ("z" #'~toggle-maximize-buffer "Toggle maximizing")
        ("p" #'ace-swap-window "Swap")
        ("p" #'ace-window "Jump")
        ("r" #'resize-window "Interactive resize")
        ("w" #'other-window "Interactive resize" :exit nil)
        ("ESC" nil "Quit"))

      (defhydra hydra-frame (:columns 4 :exit t)
        "Frame management"
        ("n" #'make-frame "New frame")
        ("k" #'delete-frame "Delete current frame"))

      (defhydra hydra-search (:columns 4 :exit t)
        "Searching"
        ("/" #'~search-buffer-interactively "Interactively search")
        ("i" #'counsel-imenu "Symbol-based")
        ("f" #'isearch-forward-regexp "Forward regexp")
        ("b" #'isearch-backward-regexp "Backward regexp")
        ("g" #'~counsel-rg "Grep")
        ("p" #'~counsel-grep-default-project-root "Grep in project"))

      (defhydra hydra-replace (:columns 4 :exit t)
        "Replacing"
        ("r" #'query-replace-regexp "Query replace regexp")
        ("q" #'query-replace "Query replace"))

      (defhydra hydra-narrow (:columns 4 :exit t)
        "Narrowing"
        ("r" #'narrow-to-region "Narrow to region")
        ("f" #'narrow-to-defun "Narrow to defun")
        ("w" #'widen "Widen the narrowed part"))

      (defhydra hydra-align (:columns 4 :exit t)
        "Aligning"
        ("a" #'align "Align")
        ("c" #'align-current "Align current section")
        ("r" #'align-regexp "Align regexp"))

      (defhydra hydra-edit (:columns 4 :exit t)
        "Editing operations"
        ("q" #'hydra-replace/body "Replacing")
        ("n" #'hydra-narrow/body "Narrowing")
        ("a" #'hydra-align/body "Align")

        ("gs" #'magit-status "Git status")
        ("gb" #'magit-blame "Git blame")
        ("gd" #'magit-diff "Git diff")

        ("vca" #'vc-annotate "VC annonate")
        ("vcd" #'vc-diff "VC diff")

        ("wo" #'just-one-space "Whitespace: Keep one")
        ("wd" #'delete-horizontal-space "Whitespace: Delete")

        ("i" #'indent-rigidly "Indent rigidly")
        ("c" #'comment-or-uncomment-region "Toggle commenting region")
        ("d" #'~duplicate-line-or-region "Duplicate current line or region")
        ("k" #'kill-sexp "Kill sexp")
        ("z" #'repeat "Repeat last command"))

      (defhydra hydra-mark (:columns 4)
        "Mark/region management"
        ("e" #'er/expand-region "Expand region")
        ("o" #'er/mark-outside-pairs "Mark outside pairs")
        ("f" #'er/mark-defun "Mark defun" :exit t)
        ("b" #'~mark-current-block "Mark block" :exit t)
        ("t" #'~mark-current-output-block "Mark output block" :exit t)
        ("SPC" #'mark-sexp "Mark sexp")
        ("ESC" nil "Quit" :exit t))

      (defhydra hydra-org-todo (:columns 4 :exit t)
        "Org TODO operations"
        ("i" #'org-insert-todo-heading "Insert TODO heading (same level)")
        ("c" #'org-todo "Change TODO state" :exit nil)
        ("s" #'org-schedule "Schedule"))

      (defhydra hydra-org-table (:columns 4 :exit t)
        "Org table operations"
        ("a" #'org-ctrl-c-ctrl-c "Table: Align"))

      (defhydra hydra-org-narrow (:columns 4 :exit t)
        "Org narrow"
        ("s" #'org-narrow-to-subtree "Narrow to subtree")
        ("b" #'org-narrow-to-block "Narrow to block")
        ("w" #'widen "Widen"))

      (defhydra hydra-org (:columns 4 :exit nil)
        "Org"
        ("o" #'hydra-org-todo/body "TODO operations" :exit t)
        ("t" #'hydra-org-table/body "Table operations" :exit t)
        ("n" #'hydra-org-narrow/body "Narrowing operations" :exit t)

        ("j" #'org-next-visible-heading "Next heading")
        ("k" #'org-previous-visible-heading "Prev heading")
        ("J" #'org-forward-heading-same-level "Next heading (same)")
        ("K" #'org-backward-heading-same-level "Prev heading (same)")

        ("g" #'org-goto "Outline-based goto" :exit t)
        ("/" #'org-sparse-tree "Create sparse tree" :exit t)

        ("m" #'org-mark-subtree "Mark subtree" :exit t)

        ("i" #'org-insert-heading "Insert heading (same level)")
        ("I" #'org-insert-item "Insert item")
        ("TAB" #'org-cycle "Tab action/Cycle")
        ("c" #'org-ctrl-c-ctrl-c "Context-based update/align")

        ("s" #'org-insert-structure-template "Insert structure template" :exit t)

        ("a" #'org-archive-subtree "Archive" :exit t)
        (">" #'org-metaright "Increase level (current)")
        ("<" #'org-metaleft "Decrease level (current)")
        ("r>" #'org-shiftmetaright "Increase level (tree)")
        ("r<" #'org-shiftmetaleft "Decrease level (tree)")
        ("P" #'org-move-subtree-up "Move up")
        ("N" #'org-move-subtree-down "Move down")

        ("ESC" nil "Quit"))

      (defhydra hydra-paredit (:columns 4)
        "Paredit mode"
        (")" #'paredit-forward-slurp-sexp "Forward slurp")
        ("(" #'paredit-backward-slurp-sexp "Backward slurp")
        ("}" #'paredit-forward-barf-sexp "Forward barf")
        ("{" #'paredit-backward-barf-sexp "Backward barf")
        ("l" #'paredit-forward "Go forward")
        ("h" #'paredit-backward "Go backward")
        ("j" #'paredit-forward-down "Go down")
        ("k" #'paredit-backward-up "Go up")

        ("es" #'paredit-split-sexp "Split" :exit t)
        ("eS" #'paredit-splice-sexp "Splice")
        ("ej" #'paredit-join-sexps "Join")

        ("ESC" nil "Quit" :exit t))

      (defhydra hydra-smartparens (:columns 4)
        "Smartparens"
        (")" #'sp-forward-slurp-sexp "Forward slurp")
        ("(" #'sp-backward-slurp-sexp "Backward slurp")
        ("}" #'sp-forward-barf-sexp "Forward barf")
        ("{" #'sp-backward-barf-sexp "Backward barf")
        ("l" #'sp-forward-sexp "Go forward")
        ("h" #'sp-backward-sexp "Go backward")
        ("j" #'sp-down-sexp "Go down")
        ("k" #'sp-backward-up-sexp "Go up")

        ("es" #'sp-split-sexp "Split" :exit t)
        ("eS" #'sp-splice-sexp "Splice")
        ("ej" #'sp-join-sexps "Join")

        ("ESC" nil "Quit" :exit t))

      (defhydra hydra-paren-edit nil
        "Paren-editing mode")
      (setq hydra-paren-edit/hint
            `(cond
              ((and (boundp 'paredit-mode) paredit-mode)
               (setq hydra-paren-edit/keymap hydra-paredit/keymap)
               (eval hydra-paredit/hint))
              (t
               (setq hydra-paren-edit/keymap hydra-smartparens/keymap)
               (eval hydra-smartparens/hint))))

      (defhydra hydra-dev-flycheck (:columns 4 :exit t)
        "Flycheck"
        ("v" #'flycheck-verify-setup "Verify setup")
        ("t" #'flycheck-mode "Toggle"))

      (defhydra hydra-dev (:columns 4 :exit t)
        "Dev"
        ("j" #'hydra-dev-clojure/body "Clojure")
        ("p" #'hydra-dev-python/body "Python")
        ("y" #'hydra-dev-flycheck/body "Flycheck")
        ("t" #'hydra-dev-tcl/body "Tcl"))

      (defhydra hydra-current-dev nil
        "Current dev mode")
      (setq hydra-current-dev/hint
            `(cond
              ((and (boundp 'cider-mode) cider-mode)
               (setq hydra-current-dev/keymap hydra-dev-clojure/keymap)
               (eval hydra-dev-clojure/hint))
              ((and (boundp 'python-mode) python-mode)
               (setq hydra-current-dev/keymap hydra-dev-python/keymap)
               (eval hydra-dev-python/hint))
              (t
               (setq hydra-current-dev/keymap hydra-emacs-lisp/keymap)
               (eval hydra-emacs-lisp/hint))))

      (defhydra hydra-global (:columns 4 :exit t)
        "Global operations"
        ("s-SPC" #'exchange-point-and-mark "Exchange point & mark")
        ("SPC" #'counsel-M-x "M-x")
        ("qq" #'delete-window "Close current window")

        ("RET" #'hydra-read-input-async/body "Read-input mode")
        ("p" #'hydra-paren-edit/body "Paren-editing mode")

        ;; Universal eval/dev mode for the current buffer
        ("e" #'hydra-current-dev/body "Dev mode for current buffer")

        ("u" #'hydra-dev/body "Dev")
        ("x" #'hydra-exec/body "Execution")
        ("l" #'hydra-emacs-lisp/body "Emacs Lisp operations")

        ("o" #'hydra-org/body "Org")

        ("/" #'hydra-search/body "Searching")
        ("d" #'hydra-edit/body "Editing")
        ("b" #'hydra-buffer/body "Buffer")
        ("c" #'hydra-cursor/body "Multiple cursors")
        ("f" #'hydra-file/body "File")
        ("j" #'hydra-jump/body "(Semantic) jumping")
        ("v" #'hydra-visit/body "Visiting")
        ("i" #'hydra-insertion/body "Insertion")
        ("t" #'hydra-format/body "Formatting")
        ("m" #'hydra-mark/body "Marking")
        ("n" #'hydra-mode/body "Mode")

        ("w" #'hydra-window/body "Window management")
        ("r" #'hydra-frame/body "Frame management"))

      ;; (bind-key "s-SPC" #'hydra-global/body)
      (bind-key "M-SPC" #'hydra-global/body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core keybindings")

(provide 'rmacs:config-core-keybindings)
