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

(require 'cl-lib)
(require 'misc)
(require 'thingatpt)
(require 'subr-x)

;; String processing
;; Ref: https://github.com/magnars/s.el
(require 's)

;; List processing, use with care since it's generally slower than Emacs Lisp primitives
;; Ref: https://github.com/magnars/dash.el
(require 'dash)

;; File/filesystem library
;; Ref: https://github.com/rejeep/f.el
(require 'f)

;; Async processing by spawning subordinate processes
;; Ref: https://github.com/jwiegley/emacs-async
(require 'async)

;; Threading (Yay!)
;; Ref: https://github.com/mola-T/timp
(require 'timp)

;; Tramp for remote & sudo access
(require 'tramp)

;; Interactive menu
;; Ref: https://github.com/abo-abo/hydra
(require 'hydra)

;; Expand selection with one keybinding
;; Ref: https://github.com/magnars/expand-region.el
(use-package expand-region
  :commands er/expand-region)

;; Enhanced find-file
(defadvice find-file (around find-files activate)
  "Also find all files within a list of files. This even works recursively."
  (if (listp filename)
      (loop for f in filename do (find-file f wildcards))
      ad-do-it))

;; Smart completion framework
;; Ref: https://github.com/abo-abo/swiper
(use-package counsel
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
              ("M-s-c"      . ~ivy-prev-line+)
              ("M-s-t"      . ~ivy-next-line+)
              ("C-m"        . ivy-alt-done)
              ("<f3>"       . ivy-occur)
              ("<s-return>" . ivy-dispatching-done)
              ("<C-return>" . ivy-immediate-done)
              ("<S-return>" . ivy-call))
  :defer nil
  :init (progn
          (require 'ivy-hydra)

          ;; Sublime-like C-p
          ;; Ref: https://github.com/vspinu/imenu-anywhere
          (require 'imenu-anywhere))
  :config (progn
            ;; Include recentf and bookmarks when switching buffers
            (setq ivy-use-virtual-buffers t)

            ;; Show 15 items
            (setq ivy-height 15)

            ;; Don't count the candidates
            (setq ivy-count-format "")

            ;; Plain display style works and is greener
            (setq ivy-display-style nil)

            ;; Use Projectile with Ivy
            (setq projectile-completion-system 'ivy)

            (setq enable-recursive-minibuffers t)

            (setq ivy-use-selectable-prompt t)

            ;; Default matching
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
            (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
            ;; (require 'flx)
            ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

            (defun* ~ivy-next-line+ (&optional (n-lines 5))
              (interactive)
              (ivy-next-line n-lines))

            (defun* ~ivy-prev-line+ (&optional (n-lines 5))
              (interactive)
              (ivy-previous-line n-lines))

            (defun ~bind-key-evil (key func)
              "Binds key in all evil states."
              (dolist (map (list evil-normal-state-map
                                 evil-insert-state-map
                                 evil-visual-state-map
                                 evil-replace-state-map
                                 evil-operator-state-map
                                 evil-motion-state-map))
                (bind-key key func map)))

            (defun* ~bind-key-with-prefix (key command &key
                                               (keymap global-map)
                                               (evil-keymap evil-normal-state-map))
              "Binds key in `evil-normal-state-map' with prefix
`SPC' and in global mode map with prefix `s-SPC' at the same
time."
              (interactive)
              (eval `(progn
                       (bind-key ,(format "s-SPC %s" key) command keymap)
                       (bind-key ,(format "SPC %s" key) command evil-keymap))))

            (defun* ~bind-key-with-prefix-local (key command &key (keymap global-map))
              "Like `~bind-key-with-prefix', except that instead
of binding to `evil-normal-state-map' it binds to
`evil-normal-state-local-map'."
              (interactive)
              (~bind-key-with-prefix key command
                                     :keymap keymap
                                     :evil-keymap evil-normal-state-local-map))

            (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
(ivy-mode 1)

;; Temporarily save points
;; Ref: https://github.com/alezost/point-pos.el
(require 'point-pos)

;; Multiple cursors
;; Ref: https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)

;; Live doc in echo area
(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          ielm-mode) . turn-on-eldoc-mode)
  :config (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

;; Custom unique naming method
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; Auto-pairing brackets
;; Ref: https://github.com/Fuco1/smartparens
;; Cheat sheet: M-x sp-cheat-sheet
(use-package smartparens-config
  :config (smartparens-global-mode))

;; Bracket-based structured editing
;; Ref: https://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          clojure-mode) . paredit-mode)
  :bind (:map paredit-mode-map
         ("s-." . paredit-backward-kill-word)
         ("s-p" . paredit-forward-kill-word)
         ("s-r" . forward-word)
         ("s-g" . backward-word)
         ("s-C" . paredit-backward-up)
         ("s-T" . paredit-forward-up)
         ("s-R" . paredit-forward)
         ("s-G" . paredit-backward))
  :config (progn
            (defun ~advice/disable-other-parens-modes-in-paredit (orig-fun &rest args)
              (when (apply orig-fun args)
                (when (fboundp 'autopair-mode)
                  (autopair-mode -1))
                (when (fboundp 'smartparens-mode)
                  (smartparens-mode -1))))
            (advice-add 'paredit-mode
                        :around #'~advice/disable-other-parens-modes-in-paredit)

            ;; Use in minibuffer
            (defun conditionally-enable-paredit-mode ()
              "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
              (if (eq this-command 'eval-expression)
                  (paredit-mode 1)))
            (add-hook 'minibuffer-setup-hook #'conditionally-enable-paredit-mode)

            ;; Use with SLIME REPL
            (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

            ;; Stop SLIME's REPL from grabbing DEL,
            ;; which is annoying when backspacing over a '('
            (defun override-slime-repl-bindings-with-paredit ()
              (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
            (add-hook 'slime-repl-mode-hook
                      'override-slime-repl-bindings-with-paredit)

            (defun ~paredit-up-all ()
              (interactive)
              (ignore-errors
                (loop do (paredit-forward-up))))))

;; Adjust indentation based on current file
;; Ref: https://github.com/jscheid/dtrt-indent
(use-package dtrt-indent
  :config (dtrt-indent-mode 1))

;; Vim mode
;; Ref: https://github.com/emacs-evil/evil
(use-package evil
  :bind (("M-ESC"     . '~keyboard-quit)
         :map evil-insert-state-map
         ("C-y"       . yank)
         ("C-o"       . ~open-line)
         ("C-w"       . kill-region)
         ("C-a"       . ~move-to-beginning-of-line)
         ("C-k"       . ~evil-kill-line-or-sexpr)
         ("<mouse-2>" . nil)
         :map evil-normal-state-map
         ("<mouse-2>" . nil)
         :map evil-visual-state-map
         ("<mouse-2>" . nil))
  :config (progn
            ;; Use insert mode by default
            (setq evil-default-state 'insert)

            (with-eval-after-load "evil-vars"
              (dolist (mode '(term-mode
                              multi-term-mode
                              ansi-term-mode
                              magit-log-edit-mode
                              magit-popup-mode
                              magit-file-mode
                              neotree-mode
                              dired-mode
                              nav-mode
                              grep-mode
                              bs-mode
                              cider-repl-mode
                              cider-popup-buffer-mode
                              cider--debug-mode
                              cider-temp-mode
                              help-mode
                              compilation-mode
                              ivy-occur-mode))
                (evil-set-initial-state mode 'emacs))
              (evil-set-initial-state 'ibuffer-mode 'normal))

            (setq evil-emacs-state-cursor 'bar)
            (setq-default cursor-type 'bar)

            ;; Better granularity for undo-tree
            (setq evil-want-fine-undo t)

            ;; Down mouse 1 should change evil to insert mode
            (defun ~advice/mouse-1-evil-insert-mode (orig-fun &rest args)
              (interactive)
              (let ((res (call-interactively orig-fun))
                    (old-point (point)))
                (call-interactively 'evil-insert)
                ;; After calling evil-insert, the cursor moves the beginning of the region
                ;; so we need to set it back
                (when (< (point) old-point)
                  (call-interactively 'exchange-point-and-mark))
                res))
            (advice-add 'evil-mouse-drag-region
                        :around #'~advice/mouse-1-evil-insert-mode)

            (defun ~evil-kill-line-or-sexpr ()
              "Kills current line or sexpr using paredit in evil mode."
              (interactive)
              (if (member* mode-name '("Emacs-Lisp" "Lisp" "Clojure") :test 'equalp)
                  (call-interactively 'paredit-kill)
                (call-interactively 'kill-line)))))
(evil-mode 1)
;; * to search forward, # to search backward
(use-package evil-visualstar
  :after (evil)
  :init (progn
          (global-evil-visualstar-mode)
          (setq evil-visualstar/persistent nil)))
(use-package evil-paredit
  :after (evil)
  :hook ((emacs-lisp-mode
          lisp-mode
          clojure-mode
          scheme-mode) . evil-paredit-mode))

;; Pattern-based command execution
;; Ref: https://github.com/cmpitg/wand
(use-package wand
  :config
  (progn
    (defun ~quick-action (text)
      "Executes special action."
      (interactive)
      (cond ((~file-pattern? text)
             (~smart-open-file text))
            ((and (derived-mode-p 'emacs-lisp-mode)
                  (s-starts-with? "(" text))
             (~eval-string text))
            (t
             (wand:eval-string text))))

    (setq wand:*rules*
          (list (wand:create-rule :match "----\n[^ ]* +"
                                  :capture :after
                                  :action ~current-snippet->file)
                (wand:create-rule :match ">\\$ "
                                  :capture :after
                                  :action erun)
                (wand:create-rule :match "> "
                                  :capture :after
                                  :action srun)
                (wand:create-rule :match "https?://"
                                  :capture :whole
                                  :action ~firefox)
                (wand:create-rule :match "file:"
                                  :capture :after
                                  :action ~smart-open-file)
                (wand:create-rule :match "#> "
                                  :capture :after
                                  :action ~add-bracket-and-eval)
                (wand:create-rule :match "mailto:"
                                  :capture :after
                                  :action (lambda (str)
                                            (~send-mail :to str)))
                (wand:create-rule :match ".*\\.html$"
                                  :capture :whole
                                  :action (lambda (path)
                                            (interactive)
                                            (~open-with path "web-browser-gui %s")))
                (wand:create-rule :match (rx (0+ (or any "\n")))
                                  :capture :whole
                                  :skip-comment nil
                                  :action ~quick-action)))))

;; Managing recent files
(use-package recentf
  :init (progn
          (recentf-mode 1)
          (setq recentf-max-menu-items 128)))

;; Displaying available keybindings in pop up
;; Ref: https://github.com/justbur/emacs-which-key
(use-package which-key
  :config (progn
            (which-key-mode)
            (which-key-setup-side-window-right-bottom)))

;; Some safe local variables
(add-to-list 'safe-local-variable-values '(local/delete-on-exit . t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished configuring core components for editing")

(provide 'rmacs:config-core-edit)
