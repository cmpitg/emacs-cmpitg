;; -*- no-byte-compile: t -*-

;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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

;; TODO: Functions:
;; - ~undo-kill-buffers
;; - ~gui/open-file
;; - ~gui/save-as
;; - ~switch-to-scratch
;; - ~new-buffer
;; - ~find-file-current-dir
;; - ~switch-to-last-buffer
;; - e b eval-buffer
;; - e e ~eval-current-expression
;; - e r eval-region
;; - e r (python-mode) python-shell-send-region
;; - g s for Magit

;; TODO: company mode
;; TODO: Make backward/forward kill word deletes, not kills

;; Doc: local/delete-on-exit
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

;; TODO - Think about C-x C-c C-v should work as expected
;; TODO: Align mode
;; TODO: Diminish WK, acme, and ,
;; TODO: temp file
;; TODO: GUI open file
;; TODO: Current directory
;; TODO: Set current project directory of all visited files
;; TODO: Make point-pos open up closed file
;; TODO: Recent file list based on Emacs server name
;; TODO: toolbox:open
;; TODO: Migrate all old advice functions to the new advice mechanism as with :commands, so force-load might be necessary

;; Test - make executable
;; Test - ~delete-current-file
;; Test - delete current file when there is a file local variable name local/delete-on-exit

;; TODO - s-; ; ;

(if (string= "1" (getenv "EMACS_FORCE_TOGGLE_DEBUG_ON_ERROR"))
    (setq debug-on-error t)
  (setq debug-on-error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important global values
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *config-dir* (file-name-directory (or (buffer-file-name)
                                                load-file-name))
  "Default configuration directory - the one containing this file")

(defvar *snippet-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "snippets"))
  "Default snippet directory.")

(defvar *scratch-dir*
  (file-name-as-directory
   (concat (file-name-as-directory *config-dir*) "scratch"))
  "Default path to Scratch directory.")

(add-to-list 'load-path *config-dir*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialized Emacs - Mail browser, ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun ~value-from-symbol (symbol)
  "Return the value that `symbol' hold if it's bound, or `nil'
otherwise."
  (if (boundp symbol)
      (symbol-value symbol)
    nil))

(defvar *emacs-as-tool* nil
  "Determine which shape Emacs is running as.")

(defvar *emacs-server-port*
  (string-to-int (or (getenv "EMACS_PORT") "9999")))

(defun ~emacs-as ()
  "Return `:mail', `:notes', :file-browser or `nil' when Emacs is
running as mail browser, note taker, or ... just Emacs the text editor."
  *emacs-as-tool*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~get-config (&rest paths)
  "Path to a config file or directory."
  (apply 'concat *config-dir* paths))

(defun ~load-files (&rest paths)
  "Loads Emacs Lisp source files when they exists."
  (dolist (file-path paths)
    (loop for possible-file-path in (list file-path
                                          (concat file-path ".el")
                                          (concat file-path ".elc"))
          when (and (file-exists-p possible-file-path)
                    (file-regular-p possible-file-path))
          do (progn (load file-path)
                    (return)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "
%s
HTTP server port: %s
Invoke debugger when error: %s
Emacs as: %s
"
         (emacs-version)
         *emacs-server-port*
         debug-on-error
         (~emacs-as))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Machine-specific configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string= "1" (string-trim (getenv "RMACS_NO_MACHINE_INIT")))
  (~load-files "~/.emacs-machine-specific-init"
               (~get-config "machine-specific-init")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager and essential library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ee:config-package-manager (~get-config "config-package-manager"))

(require 'cl-lib)
(require 'misc)
(require 'thingatpt)

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

            (bind-key "s-+" 'mc/edit-lines)
            (~bind-key-with-prefix "")

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
;; Sane behaviors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 3 lines at a time normally, 5 lines at a time with shift
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))

;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

;; Keyboard scroll 3 lines at a time
(setq scroll-step 3)
(setq scroll-conservatively 10000)

;; Restore cursor position after scrolling
;; Ref: http://elpa.gnu.org/packages/scroll-restore.html
(use-package scroll-restore
  :config (progn
            ;; Recenter when jumpting back
            (setq scroll-restore-center t)

            ;; Allow scroll-restore to modify the cursor face
            (setq scroll-restore-handle-cursor t)

            ;; Make the cursor invisible while POINT is off-screen
            (setq scroll-restore-cursor-type nil)

            ;; Jump back to the original cursor position after scrolling
            (setq scroll-restore-jump-back t)

            ;; Due to some reason the mode needs disabling and re-enabling to
            ;; work
            (scroll-restore-mode -1)
            (scroll-restore-mode 1)))

;; Disable Tramp autosave
(setq tramp-auto-save-directory "/tmp/")

;; Don't let the cursor go into minibuffer prompt
;; Ref: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; Use the same clipboard with X
(setq x-select-enable-clipboard t)

;; Echo when trying to kill in a read-only buffer
(setq kill-read-only-ok t)

;; Always suggest keybindings
(setq suggest-key-bindings t)

;; More tolerable stack
(setq max-lisp-eval-depth 15000)
(setq max-specpdl-size    15000)

;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Maximum number of ring items to store
(setq mark-ring-max 512)

;; yes/no questions become y/n questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backspace and Del delete selection, except in paredit-mode
(delete-selection-mode 1)

;; Set the default tab width
(setq default-tab-width 4)

;; Set tab width
(setq tab-width 4)

;; Default tab-size for C
(setq-default c-basic-offset 4)

;; Expand tabs to spaces
(setq-default indent-tabs-mode nil)

;; Subword should include camelCase notation
(global-subword-mode 1)

;; Hide the toolbar
(tool-bar-mode -1)

;; 1 use mouse, so scroll bar comes in handy
;; (scroll-bar-mode -1)
(scroll-bar-mode 1)
(set-scroll-bar-mode 'left)

;; (menu-bar-mode -1)
(menu-bar-mode 1)

;; Display time
;; (display-time)
(display-time-mode -1)

;; Turn off welcome message
(setq inhibit-startup-message t)

;; Display the size of the buffer
(size-indication-mode 1)

;; Change cursor type
;; (set-default 'cursor-type 'hbar)
;; (set-default 'cursor-type 'box)
(set-default 'cursor-type 'bar)

;; Show matching parentheses
(show-paren-mode 1)

;; Set printing type
(setq ps-paper-type 'a4)

;; Disable backup file
(setq make-backup-files nil)

;; Show column number
(column-number-mode 1)

;; Turn on the search-highlighting
(setq search-highlight 1)

;; Case-insensitive searching
(setq-default case-fold-search t)
(setq case-fold-search t)

;; Ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; Dim the ignored part of the file name
(file-name-shadow-mode 1)

;; Minibuffer window expands vertically as necessary to hold the text
;; that you put in the minibuffer
(setq resize-mini-windows t)

;; Line numbering is off by default
(use-package linum
  :init (progn
          (global-linum-mode -1)
          (setq linum-format "%d ")))

;; Don't highlight the current line
(hl-line-mode -1)

;; Added functionality to hippie-expand
(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;; Hide undo-tree from mode line
(use-package undo-tree
  :diminish undo-tree-mode)

;; Make shebang-ed files executable
(add-hook 'after-save-hook '~maybe-make-current-file-executable)

;; Clean up all Tramp remote connection before killing Emacs
(add-hook 'kill-emacs-hook '~clean-up-tramp)

;; Delete file when killing buffer if necessary
(add-hook 'kill-buffer-hook #'~maybe-delete-file-when-killing-buffer)

;; Focus follows mouse
(setq mouse-autoselect-window t)

;; Set frame title
(let ((title-format
       `("Rmacs"
         ,(format " @ %s" (or *emacs-as-tool*
                              :edit))
         " \u262f "
         (buffer-file-name "%f"
                           (dired-directory dired-directory "%b")))))
  (setq-default frame-title-format title-format)
  (setq frame-title-format title-format))

;; Soft-wrapping long lines
;; Ref: https://github.com/joostkremers/visual-fill-column
(use-package visual-fill-column
  :init (progn
          (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)

          ;; Correct the default split
          (setf split-window-preferred-function
                #'visual-fill-column-split-window-sensibly)

          (with-eval-after-load "evil"
            ;; Make movement keys work like they should
            (define-key evil-normal-state-map
              (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
            (define-key evil-normal-state-map
              (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
            (define-key evil-motion-state-map
              (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
            (define-key evil-motion-state-map
              (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
            ;; Make horizontal movement cross lines
            (setq-default evil-cross-lines t))))

;; Preventing other windows from stealing the current window
(defadvice pop-to-buffer (before cancel-other-window first)
  (ad-set-arg 1 nil))
(ad-activate 'pop-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Menu
;;

(defun ~right-click-menu ()
  "Returns a list to build a context menu."
  `(""
    ["Cut" clipboard-kill-region (~is-selecting?)]
    ["Copy" kill-ring-save (~is-selecting?)]
    ["Paste" yank t]
    ["Delete" delete-region (~is-selecting?)]
    ["--" ignore]
    ["Exec (other window)" ~exec-in-other-window (~is-selecting?)]
    ["Exec in Tmux" emamux:send-region (~is-selecting?)]
    ["--" ignore]
    ["Undo" undo-tree-undo t]
    ["Redo" undo-tree-redo t]
    ["--" ignore]))

;;
;; Region: https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Region.html
;;

(defalias '~is-selecting? 'use-region-p
  "Determines if current there is a selection/active region.")
(defalias '~selection-start 'region-beginning)
(defalias '~selection-end 'region-end)

(defun ~current-selection ()
  "The currently selected text."
  (if (~is-selecting?)
    (buffer-substring (~selection-start)
                      (~selection-end))
    ""))

(defun ~get-secondary-selection ()
  "Gets the secondary selection (by default, activated with `M-Mouse-1')."
  (x-get-selection 'SECONDARY))

;;
;; Text wrapping
;;

(defun ~turn-on-soft-wrapping ()
  "Turns on soft-wrapping."
  (interactive)
  (turn-off-auto-fill)
  (turn-on-visual-line-mode)
  (turn-on-visual-fill-column-mode))

(defun ~turn-off-soft-wrapping ()
  "Turns off soft-wrapping."
  (interactive)
  (visual-line-mode -1)
  (visual-fill-column-mode -1))

;;
;; Editing
;;

(defun ~open-line (arg)
  "Opens line and moves to the next line."
  (interactive "p")
  (end-of-line)
  (delete-horizontal-space)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(defun ~open-line-before (arg)
  "Opens line and moves to the previous line."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

;; TODO cleanup
(defun ~duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.

With argument N, make N copies.
With negative N, comment out original line and use the absolute value.

Source: http://stackoverflow.com/a/4717026/219881"
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next
                                        ; line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not
                                        ; specified
          (insert text))))
    (if use-region nil              ; Only if we're working with a line (not a
                                        ; region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                     ; Comment out original with negative
                                        ; arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun ~move-to-beginning-of-line ()
  "Moves point back to indentation of beginning of line.

Moves point to the first non-whitespace character on this line.
If point is already there, moves to the beginning of the line."
  (interactive)
  (let ((orig-point (point)))
    (unless visual-line-mode
      (back-to-indentation))
    (when (= orig-point (point))
      (beginning-of-visual-line nil))))

(defun* ~previous-line+ (&optional (n-lines 5))
  "Scrolls up `n-lines'."
  (interactive)
  (previous-line n-lines))

(defun* ~next-line+ (&optional (n-lines 5))
  "Scrolls down `n-lines'."
  (interactive)
  (next-line n-lines))

(defun ~mark-line ()
  "Marks current line."
  (interactive)
  (beginning-of-line)
  (push-mark (point) t t)
  (end-of-line))

(defun* ~file-pattern? (str &key (must-exists t))
  "Determines if a string is a file pattern \(`path' or
`path:line-number', or `path:pattern'\).  By default, the
corresponding file must exist for this function to return `t'.
To remove this constraint, pass in `:must-exists nil'.  E.g.

\(~file-pattern? \"/tmp/aoeu\"\)                                        ⇒ t
\(~file-pattern? \"/tmp/aoeu:10\"\)                                     ⇒ t
\(~file-pattern? \"/tmp/aoeu:/hello world/\"\)                          ⇒ t
\(~file-pattern? \"/tmp/non-existent\"\)                                ⇒ nil
\(~file-pattern? \"/tmp/non-existent\" :must-exists nil\)               ⇒ t
\(~file-pattern? \"/tmp/non-existent:10\" :must-exists nil\)            ⇒ t
\(~file-pattern? \"/tmp/non-existent:/hello world/\" :must-exists nil\) ⇒ t
"
  (cl-flet ((check-file-exists? (path) (if must-exists
                                           (f-exists? path)
                                         t)))
    (let ((str (s-trim str)))
      (or (check-file-exists? str)
          (let ((components (s-split ":" str)))
            (and (= 2 (length components))
                 (check-file-exists? (first components))))))))

(defun ~deconstruct-path (path)
  "Deconstructs a path notation into `path' and `number' or
`pattern'.  See the following examples for further information:

\(~deconstruct-path \"/tmp/aoeu\"\)                       ⇒ \(values \"/tmp/aoeu\"\)
\(~deconstruct-path \"/tmp/aoeu:10\"\)                    ⇒ \(values \"/tmp/aoeu\" 10\)
\(~deconstruct-path \"/tmp/aoeu:10/other/:20\"\)          ⇒ \(values \"/tmp/aoeu:10/other/:20\" 20\)
\(~deconstruct-path \"/tmp/aoeu:/hello world/\"\)         ⇒ \(values \"/tmp/aoeu\" \"hello world\"\)
\(~deconstruct-path \"/tmp/aoeu:/inside:/hello world/\"\) ⇒ \(values \"/tmp/aoeu:/inside\" \"hello world\"\)
"
  (let ((matches (or (s-match (rx (group (one-or-more any))
                                  ":" (group (one-or-more digit))
                                  eol)
                              path)
                     (s-match (rx (group (one-or-more any))
                                  ":/" (group (one-or-more any)) "/" eol)
                              path))))
    (if matches
        (let* ((path (nth 1 matches))
               (pattern-or-number (nth 2 matches))
               (number (string-to-int pattern-or-number)))
          (if (zerop number)
              (values path pattern-or-number)
            (values path number)))
      (values path))))

(defun ~insert-full-line-comment ()
  "Inserts a line full of comment characters until `fill-column' is reached."
  (interactive)
  (let ((comment (s-trim comment-start)))
    (->> (loop for time from (current-column) upto (1- fill-column) by (length comment)
               collect comment)
         (s-join "")
         insert)))

(defun ~keyboard-quit ()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we
clicked away or set the cursor into another buffer) we can quit
by pressing 'ESC' three times. This function handles it more
conveniently, as it checks for the condition of not beign in the
minibuffer but having it active. Otherwise simply doing the ESC
or (keyboard-escape-quit) would brake whatever split of windows
we might have in the frame."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (if (or mark-active (active-minibuffer-window))
          (keyboard-escape-quit))
    (keyboard-quit)))

;;
;; Window
;;

(defalias '~one-window 'delete-other-windows
  "Makes current window the only window visible.")

(defun ~delete-window ()
  "Delete current window if it's not sticky/dedicated.  Use
prefix arg (`C-u') to force deletion if it is."
  (interactive)
  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Window '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))
      (delete-window (selected-window))))

;;
;; File & buffer
;;

(defun* ~smart-open-file (path &key (new-frame? nil))
  "Opens path and with external program if necessary."
  (dolist (regexp&action (append (if (boundp '*open-with-regexps*)
                                     *open-with-regexps*
                                   (list))
                                 (list '(".*" . (lambda (path)
                                                  (~open-file-specialized path
                                                                          :new-frame? new-frame?))))))
    (let ((regexp (car regexp&action))
          (action (cdr regexp&action)))
      (when (s-matches-p regexp path)
        (return (typecase action
                  (function   (funcall action path))
                  (string     (~open-with path action))
                  (otherwise  (message-box (format "Invalid program %s" action)))))))))

(defun ~open-with (file cmd)
  "Opens file with a command line."
  (~run-process (format cmd file)))

(defun ~find-file-new-frame (path &optional wildcards)
  "Calls `find-file' in a new frame."
  (let ((frame (make-frame)))
    (select-frame frame)
    (find-file path wildcards)))

(defun* ~open-file-specialized (file-pattern &key (new-frame? nil))
  "Opens a path and jumps to a line based on number or a the
first occurrence of a pattern.  E.g.

* If `file-pattern' is a path, open it;

* If `file-pattern' is of format \"<path>:<number>\", open the
  file and jump to the corresponding line number;

* If `file-pattern' is of format \"<path>:/<pattern>/\", open the
  file and jump to the first occurrence of `pattern'.
"
  (multiple-value-bind (path pattern)
      (~deconstruct-path file-pattern)
    (if new-frame?
        (~find-file-new-frame path)
      (find-file path))
    (when pattern
      (cond ((numberp pattern)
             (goto-line pattern))
            (t
             (beginning-of-buffer)
             (re-search-forward pattern))))
    path))

(defun ~delete-current-file ()
  "Deletes the file associated with the current buffer and kills
off the buffer."
  (interactive)
  (let ((current-file buffer-file-name))
    (when (yes-or-no-p (concat "Delete file: " current-file))
      ;; Prevent the following kill-buffer from recursively calling this
      ;; function
      (when (local-variable-p 'local/delete-on-exit)
        (kill-local-variable 'local/delete-on-exit))
      (kill-buffer (current-buffer))

      (delete-file current-file)
      (message "%s deleted" current-file))))

(defun ~maybe-delete-file-when-killing-buffer ()
  "Deletes current file when killing buffer if needed."
  (interactive)
  (when (and (local-variable-p 'local/delete-on-exit)
             local/delete-on-exit)
    (~delete-current-file)))

(defun ~maybe-make-current-file-executable ()
  "Checks for the hashbang and `chmod u+x`s current file if
needed."
  (interactive)
  (and
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (save-match-data
         (looking-at "^#!/"))))
   (not (file-executable-p buffer-file-name))
   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
   (revert-buffer)
   (message "%s saved as executable" buffer-file-name)))

(defun ~clean-up-tramp ()
  "Closes all tramp connections and buffers."
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers))

;;
;; Emacs Lisp
;;

(defun ~eval-string (str)
  "Evals a string."
  (interactive "sString: ")
  (eval (first (read-from-string (concat "(progn " str ")")))))

(defun ~eval-last-sexp-or-region ()
  "Evals region if active, or evals last sexpr"
  (interactive)
  (if (~is-selecting?)
      (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(defun ~read-command-or-get-from-secondary-selection ()
  "Without prefix argument, if there is an active selection,
returns it (assuming that it denotes a shell command); otherwise,
reads and returns a shell command from the minibuffer.

With prefix argument, always reads the shell command from the
minibuffer."
  (interactive)
  (if (and (~get-secondary-selection) (not current-prefix-arg))
      (~get-secondary-selection)
    (read-shell-command "Command: ")))

;;
;; Processes
;;

;; TODO review
(defun* ~run-process (cmd &key (async t))
  "Runs an external process.  If `async' is non-`nil' The process
is not terminated when Emacs is and the output is discarded;
otherwise, both output from stdout and stderr are direceted to
the \"*Messages*\" buffer. `cmd' is executed via `dash -c'."
  (call-process "dash"
                nil
                (if async 0 "*external-process*")
                nil
                "-c"
                cmd))

(defun ~exec (command)
  "Executes a shell command then returns its value as string."
  (interactive "MCommand: ")
  (with-temp-buffer
    (shell-command command t nil)
    (buffer-string)))

(defun ~exec-in-other-window (&optional command)
  "Executes with other-window."
  (interactive)
  (when (null command)
    (if (~is-selecting?)
        (setq command (~current-selection))
      (setq command (read-shell-command "Command: "))))
  (shell-command command))

;; RIGHT HERE

(defun ~exec< (&optional command)
  "Executes a shell command and pipes the output to the current
buffer.  If there is an active secondary selection active, the
command is the selection string; otherwise, it is read from the
minibuffer.

With prefix argument, always reads command from the minibuffer."
  (interactive)
  (let ((command (or command (~read-command-or-get-from-secondary-selection))))
    (shell-command command t)))

(defun ~exec| (&optional command)
  "Executes a shell command and pipes the output to the current
buffer.  If there is an active primary selection, it is piped as
input to the command and the output from the command would
replace the selection.  If there is an active secondary selection
active, the command is the selection string; otherwise, it is
read from the minibuffer."
  (interactive)
  (let ((command (or command
                     (~read-command-or-get-from-secondary-selection))))
    (if (~is-selecting?)
        (shell-command-on-region (region-beginning)
                                 (region-end)
                                 command
                                 t
                                 t
                                 nil)
      (shell-command command t))))

;; TODO review
(defun ~exec> (&optional command)
  "Executes a shell command and pipes the output a pop-up buffer
named \"*Shell Output*\".  If there is an active secondary
selection active, the command is the selection string; otherwise,
it is read from the minibuffer."
  (interactive)
  (let ((command (or command
                     (~read-command-or-get-from-secondary-selection))))
    (get-buffer-create "*Shell Output*")
    (with-current-buffer "*Shell Output*"
      (insert (shell-command-to-string command)))
    (~popup-buffer "*Shell Output*")))

(defun ~exec|-select-output ()
  "Calls `~exec|'.  After the output has been piped in to the
buffer, select it."
  (interactive)
  (let ((marker-start (copy-marker (or (region-beginning) (point)) nil))
        (marker-end (copy-marker (or (region-end) (point)) t)))
    (call-interactively '~exec|)
    (deactivate-mark t)
    (set-mark marker-end)
    (goto-char marker-start)
    (setq deactivate-mark nil)))

(defun ~exec<-select-output (&optional command)
  "Calls `~exec<'.  After the output has been piped in to the
buffer, select it."
  (interactive)
  (let ((marker-start (copy-marker (point) nil))
        (marker-end (copy-marker (point) t)))
    (call-interactively '~exec<)
    (set-mark marker-end)
    (goto-char marker-start)
    (setq deactivate-mark nil)))

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
  (bind-key "<C-S-t>" '~undo-kill-buffers)

  ;; Semantic editting
  (bind-key "s-\\" 'counsel-imenu)
  (~bind-key-evil "C-p" 'ivy-imenu-anywhere)

  ;; Window
  (bind-key "C-%" '~one-window)
  (bind-key "C-7" 'split-window-vertically)
  (bind-key "C-5" 'split-window-horizontally)
  (~bind-key-with-prefix "o w" '~one-window)

  ;;
  ;; Emacs Lisp
  ;;

  (bind-key "<C-return>" '~eval-last-sexp-or-region emacs-lisp-mode-map)
  (bind-key "<M-return>" 'eval-defun emacs-lisp-mode-map)
  (~bind-key-with-prefix "h ." 'find-function emacs-lisp-mode-map)

  ;;
  ;; Function keys & other convenient bindings
  ;;

  (bind-key "<C-f1>" '~switch-to-scratch)
  (bind-key "<f2>" 'save-buffer)
  (bind-key "<S-f2>" '~gui/save-as)
  (bind-key "<C-f2>" 'point-pos-save)
  (bind-key "<f3>" 'counsel-find-file)
  (bind-key "<C-f3>" '~find-files-current-dir)
  (bind-key "<S-f3>" 'projectile-find-file)
  (bind-key "<C-f4>" 'kill-buffer)
  (bind-key "<S-f4>" '~delete-window)
  (bind-key "<f8>" 'ivy-switch-buffer)
  (bind-key "<S-f8>" 'counsel-bookmark)
  (bind-key "<f11>" 'counsel-yank-pop)
  (bind-key "<f12>" 'counsel-M-x)

  (bind-key "s-SPC s-SPC" 'exchange-point-and-mark)
  (~bind-key-with-prefix "m e" 'er/mark-outside-pairs)
  (~bind-key-with-prefix "q q" 'save-buffers-kill-emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ee:config-ipc (~get-config "config-ipc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Done loading Rmacs bare")
