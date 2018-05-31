;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linum mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Line numbering is off by default
(use-package linum
  :init (progn
          (global-linum-mode -1)
          (setq linum-format "%d ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The messy Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :config
  (use-package helm
    :config (progn
              (helm-mode 1)
              (setq helm-boring-buffer-regexp-list '("\\*.+\\*"))

              ;; Exiting minibuffer ASAP so that it won't cause problem when
              ;; typing fast
              (setq helm-exit-idle-delay 0)

              ;; The following call to helm-follow-mode is local and has no
              ;; effect.  Read its documentation for the possible effect.
              ;; (helm-follow-mode 1)

              ;;
              ;; helm-follow currently comes with a cost that it opens every
              ;; file user is selecting in the Helm buffer. Add this when
              ;; you're absolutely sure you would open all files when visiting
              ;; them.
              ;;
              ;; (add-hook 'helm-after-update-hook #'(lambda ()
              ;;                                       (helm-follow-mode 1)))

              ;; Follow mode
              (eval-after-load "helm-multi-occur-1"
                '(progn
                   (helm-attrset 'follow 1 helm-source-moccur)))

              (eval-after-load "helm-occur-from-isearch"
                '(progn
                   (helm-attrset 'follow 1 helm-source-occur)))

              (eval-after-load "helm-occur"
                '(progn
                   (helm-attrset 'follow 1 helm-source-occur)))

              ;; Stupid bug makes me comment this line, why????
              (eval-after-load "helm-buffers-list"
                '(progn
                   (eval-after-load "helm-buffers"
                     '(progn
                        (helm-attrset 'follow
                                      1
                                      helm-source-buffers-list)))))

              (eval-after-load "helm-bookmark"
                '(progn
                   (helm-attrset 'follow 1 helm-source-bookmarks)))

              ;; Don't auto change-dir when find-file
              (setq-default helm-ff-auto-update-initial-value nil)

              ;; Fully enable fuzzy matching
              (setq helm-mode-fuzzy-match t
                    helm-completion-in-region-fuzzy-match t)

              ;;
              ;; Setting up Helm follow mode
              ;;
              ;; This is why I hate Helm developer:
              ;; https://github.com/emacs-helm/helm/issues/530
              ;;

              (helm-occur-init-source)

              (helm-attrset 'follow 1 helm-source-occur)
              (helm-attrset 'follow 1 helm-source-regexp)
              ;; (helm-attrset 'follow 1 helm-source-moccur)

              (bind-key "<backtab>" 'helm-execute-persistent-action helm-map)
              (bind-key "<f12>" 'helm-minibuffer-history minibuffer-local-map)
              (bind-key "<S-f1>" 'helm-minibuffer-history minibuffer-local-map)

              (when (executable-find "curl")
                (setq helm-google-suggest-use-curl-p t))

              ;; Open Helm buffer inside current window, do not disturb other
              ;; window
              (setq helm-split-window-in-side-p t)

              ;; Make Helm items cycleable
              ;; (setq helm-move-to-line-cycle-in-source t)

              ;; Search for library in `require' and `declare-function'
              ;; (setq helm-ff-search-library-in-sexp t)

              ;; Some fuzzy matching option
              ;; (setq helm-M-x-fuzzy-match t)
              ;; (setq helm-buffers-fuzzy-matching t)
              ;; (setq helm-recentf-fuzzy-match t)
              ;; (setq helm-semantic-fuzzy-match t)
              ;; (setq helm-imenu-fuzzy-match t)
              ;; (setq helm-locate-fuzzy-match t)
              ;; (setq helm-apropos-fuzzy-match t)
              ;; (setq helm-lisp-fuzzy-completion t)

              (setq helm-ff-file-name-history-use-recentf t))))

;; Integration with Helm completion framework
(use-package helm-projectile
  :ensure t
  :disabled t
  :config
  (progn
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)
    (setq projectile-require-project-root t)
    (setq projectile-switch-project-action 'projectile-dired)

    ;; A bug in projectile ignore that doesn't ignore
    ;; (setq projectile-indexing-method 'native)
    ))

(with-eval-after-load "company"
  (use-package helm-company
    :ensure t
    :config (progn
              (bind-key "M-RET" 'helm-company company-mode-map)
              (bind-key "M-RET" 'helm-company company-active-map))))

(with-eval-after-load "clojure-mode"
  ;; https://github.com/clojure-emacs/helm-cider
  (use-package helm-cider
    :ensure t
    :config (helm-cider-mode 1)))

;;
;; Use for one-off searches (as the Helm buffer is not persistent)
;;

(use-package helm-ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better grep'ing with ag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AckFull, wgrep, ack-and-a-half are all abandonware.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Use interactively
;;

;; (load-file (~get-config "local-packages/ack-and-a-half/ack-and-a-half.el"))
;; (use-package ack-and-a-half
;;   :init (progn
;;           ;; Fix Debian-based distros' executable file
;;           (setq ack-and-a-half-executable (or (executable-find "ack-grep")
;;                                               (executable-find "ack")))
;;           ;; (setq ack-and-a-half-prompt-for-directory 'unless-guessed)
;;           (setq ack-and-a-half-prompt-for-directory t)
;;           (defalias 'ack 'ack-and-a-half)
;;           (defalias 'ack-same 'ack-and-a-half-same)
;;           (defalias 'ack-find-file 'ack-and-a-half-find-file)
;;           (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

;; (use-package ag
;;   :ensure t)

;; (use-package wgrep-ag
;;   :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Workaround: virtualenvwrapper.el needs to be loaded explicitly
;; (unless (package-installed-p 'virtualenvwrapper)
;;   (package-install 'virtualenvwrapper))
;; (load-file (~get-library-full-path "virtualenvwrapper"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package ido
;;   :ensure ido
;;   :config (progn
;;             (ido-mode 1)
;;             (ido-everywhere 1)
;;             (use-package flx-ido
;;               :config (progn
;;                         (flx-ido-mode 1)

;;                         ;; disable ido faces to see flx highlights.
;;                         (setq ido-use-faces nil)))
;;             (use-package ido-vertical-mode
;;               :ensure ido-vertical-mode
;;               :config (ido-vertical-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex - A better M-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package smex
;;   :ensure t
;;   :init (progn
;;           (smex-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple tabbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package tabbar-ruler
;;   :ensure tabbar-ruler
;;   :config (progn
;;             (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
;;             ;; (setq tabbar-ruler-global-ruler t)     ; if you want a global ruler
;;             ;; (setq tabbar-ruler-popup-menu t)       ; If you want a popup menu.
;;             ;; (setq tabbar-ruler-popup-toolbar t)    ; If you want a popup toolbar
;;             ;; (setq tabbar-ruler-popup-scrollbar t)) ; If you want to only show
;;             ;;                                        ; the scroll bar when your
;;             ;;                                        ; mouse is moving.
;;             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq redisplay-dont-pause t
;;       scroll-margin 1
;;       scroll-step 3
;;       scroll-conservatively 10000
;;       scroll-preserve-screen-position 1)

;; (setq scroll-margin 1
;;       ;; scroll-conservatively 0
;;       scroll-conservatively 10000
;;       scroll-up-aggressively 0.01
;;       scroll-down-aggressively 0.01)
;; (setq-default scroll-up-aggressively 0.01
;;               scroll-down-aggressively 0.01)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP-based IPC with emnode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq *emnode-routes*
;;       '(("^.*//eval/?"         . ~ipc-eval)
;;         ("^.*//open/\\(.*\\)"  . ~ipc-open-file)
;;         ("^.*//exec/\\(.*\\)"  . ~ipc-exec-file)))

;; (defun ~ipc-eval (httpcon)
;;   (let* ((expr (format "%s" (emnode:http-data httpcon))))
;;     (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
;;     (unless (~string-empty? (s-trim expr))
;;       (emnode:http-end httpcon (or (ignore-errors (format "%s" (~add-bracket-and-eval expr)))
;;                                    "")))))

;; (defun ~ipc-open-file (httpcon)
;;   (let ((path (emnode:http-get-arg httpcon 1)))
;;     (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
;;     (emnode:http-end httpcon (format "Opening: %s\n" path))
;;     (toolbox:open-file path :new-frame? (emnode-http-param httpcon "new-frame"))))

;; (defun ~ipc-exec-file (httpcon)
;;   (let ((path (emnode:http-get-arg httpcon 1)))
;;     (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
;;     (emnode:http-end httpcon (format "Executing: %s" path))
;;     (with-temp-buffer
;;       (insert-file-contents path)
;;       (eval-buffer))))

;; ;; $ curl 0:9999/eval/ -d 'message-box "Hello World"'
;; ;; $ curl 0:9999/eval -d '(message-box "Hello") (message-box "World")'
;; ;; $ curl 0:9999/open//m/src
;; ;; $ curl 0:9999/exec//tmp/tmp.el

;; (eval-and-compile
;;   (defun cmpitg/emnode-load-path ()
;;     (~get-config "local-packages/emnode")))

;; (use-package emnode
;;   :load-path (lambda () (list (cmpitg/emnode-load-path)))
;;   :ensure elnode
;;   :config
;;   (progn
;;     (setq emnode:*log-level* emnode:+log-none+)
;;     (emnode:stop *emacs-server-port*)
;;     (ignore-errors
;;       (emnode:start-server *emnode-routes* :port *emacs-server-port*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep region after copying
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Old way
;; (defadvice kill-ring-save (after keep-transient-mark-active ())
;;   "Overrides the deactivation of the mark."
;;   (setq deactivate-mark nil))
;; (ad-activate 'kill-ring-save)

(defun ~advice/keep-transient-mark-active (orig-fun &rest args)
  "Overrides the deactivation of the mark."
  (setq deactivate-mark nil)
  (apply orig-fun args)
  (setq deactivate-mark nil))
(advice-add 'kill-ring-save
            :around #'~advice/keep-transient-mark-active)
(advice-add 'evil-paredit-yank
            :around #'~advice/keep-transient-mark-active)
(advice-add 'evil-yank
            :around #'~advice/keep-transient-mark-active)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-complete mode: https://github.com/auto-complete/auto-complete
;;
;; Comparison:
;;
;; * https://github.com/company-mode/company-mode/issues/68
;;
;; * Company uses pop-tip, much more usable than pop-up.el which Auto-complete
;;   is using.
;;
;; * Simpler to setup, "just-work", less edge cases
;;
;; * Simpler configuration
;;
;; * Well-thought APIs, backends and frontends are easy to write and integrate
;;

;; (use-package auto-complete
;;   :diminish auto-complete-mode
;;   :ensure t
;;   :init (progn
;;           (require 'auto-complete-config)
;;           (ac-config-default)
;;           (setq ac-sources
;;                 '(ac-source-filename
;;                   ac-source-functions
;;                   ;; ac-source-yasnippet
;;                   ac-source-variables
;;                   ac-source-symbols
;;                   ac-source-features
;;                   ac-source-abbrev
;;                   ac-source-words-in-same-mode-buffers
;;                   ac-source-dictionary))

;;           (auto-complete-mode 1)
;;           (setq ac-fuzzy-enable t)

;;           (add-hook 'ruby-mode-hook
;;                     (lambda ()
;;                       (add-to-list 'ac-sources 'ac-source-rsense-method)
;;                       (add-to-list 'ac-sources 'ac-source-rsense-constant)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tag management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -e -R %s"
           (executable-find "ctags")
           (directory-file-name dir-name))))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo
find-tag.  If buffer is modified, ask about save before running
etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them
silently."
  (interactive)
  (shell-command (format "%s -e -R *.%s"
                         *ctags-path*
                         (or extension "*")
                         ))
  (let ((tags-revert-without-query t)) ; don't query, revert silently
    (visit-tags-table default-directory nil)))

(defun ~goto-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Document me
(dolist (symb '(use-package))
  (put symb 'lisp-indent-function 1)
  (put symb 'common-lisp-indent-function 1)
  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,(~symbol->string symb) . font-lock-keyword-face))))

;; (custom-set-variables
;;  '(face-font-family-alternatives (quote (("Monaco" "Consolas" "Monospace")
;;                                          ("Monaco" "Consolas" "CMU Typewriter Text" "fixed")
;;                                          ("Geneva" "Sans Serif" "helv" "helvetica" "arial" "fixed")
;;                                          ("helv" "helvetica" "arial" "fixed"))))
;;  '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10) (encoding . utf-8))))
;;  '(show-paren-mode t)
;;  '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; (custom-set-faces
;;  '(default ((t (:inherit nil
;;                          :stipple nil
;;                          :inverse-video nil
;;                          :box nil
;;                          :strike-through nil
;;                          :overline nil
;;                          :underline nil
;;                          :slant normal
;;                          :weight normal
;;                          :height 100
;;                          :width normal
;;                          :foundry "unknown"
;;                          :family "Monaco"))))
;;  '(rst-level-1-face ((t (:embolden t))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup mode-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Install from local
;;
;; https://github.com/jonathanchu/emacs-powerline
;;

;; (load-file (~get-config "local-packages/powerline/powerline.el"))
;; (use-package powerline)

;; https://github.com/milkypostman/powerline
;; (use-package powerline
;;   :ensure t
;;   :init (powerline-default-theme))

;; https://github.com/Dewdrops/powerline
(eval-and-compile
  (defun cmpitg/powerline-load-path ()
    (~get-config "local-packages/drewdrops-powerline")))

(use-package powerline
  :load-path (lambda () (list (cmpitg/powerline-load-path)))
  :config (progn
            (powerline-default-theme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better popup window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/m2ym/popwin-el
;; FIXME: Reevaluate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package popwin
;;   :ensure t
;;   :config (progn
;;             (popwin-mode 1)
;;             (setq anything-samewindow nil)

;;             (dolist (el '(("\*anything*" :regexp t :height 25)
;;                           ("*anything*" :height 20)
;;                           (dired-mode :position top)
;;                           "*Backtrace*"
;;                           "*Shell Command Output*"
;;                           (compilation-mode :noselect t)
;;                           "*slime-apropos*"
;;                           "*slime-macroexpansion*"
;;                           "*slime-description*"
;;                           ("*slime-compilation*" :noselect t)
;;                           "*slime-xref*"
;;                           (sldb-mode :stick t)
;;                           slime-repl-mode
;;                           slime-connection-list-mode
;;                           "*vc-diff*"
;;                           "*vc-change-log*"
;;                           (" *undo-tree*" :width 0.3 :position right)
;;                           ("^\*helm.+\*$" :regexp t :height 20)))
;;               (push el popwin:special-display-config))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package menu-bar+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Side-bar directory tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; https://github.com/sabof/project-explorer
;;
;; (use-package project-explorer
;;   :ensure t)

;;
;; Nav is tested, good enough to go
;;

;; https://github.com/ancane/emacs-nav

;; (use-package nav
;;   :load-path "/m/src/emacs-nav"
;;   :config (progn
;;             (nav-disable-overeager-window-splitting)))
;;           (progn

;; (eval-after-load "evil-mode"
;;   '(progn
;;      (bind-key "SPC o a" 'nav-toggle evil-normal-state-map)))

;;
;; Neotree is extremely buggy and under-maintained
;;
;; https://github.com/jaypei/emacs-neotree
;; https://www.emacswiki.org/emacs/NeoTree
;;

;; (use-package neotree
;;   :ensure t
;;   :config
;;   (progn
;;     ;; (setq neo-theme 'icons)
;;     (setq neo-theme 'arrow)

;;     (add-hook 'neotree-mode-hook
;;               (lambda ()
;;                 (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
;;                 (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
;;                 (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;;                 (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)))

;;     ;; If you use popwin, when NeoTree is open and successively a temporary
;;     ;; buffer is opened with popwin, a new window with the NeoTree buffer is
;;     ;; displayed side by side next to the first one (#50). This code will help
;;     ;; you

;;     ;; (when neo-persist-show
;;     ;;   (add-hook 'popwin:before-popup-hook
;;     ;;             (lambda () (setq neo-persist-show nil)))
;;     ;;   (add-hook 'popwin:after-popup-hook
;;     ;;             (lambda () (setq neo-persist-show t))))
;;     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Soft-wrap long lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In text-related modes only, don't mess up with code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; I have tried adaptive-wrap but didn't like it.  It gave false impression of
;; how a line is actually wrapped.  The code doesn't handle different cases as
;; nicely as visual-fill-column
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/boyw165/history
;; Very buggy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package history
;;   :ensure t
;;   :diminish history-mode
;;   :config (progn
;;             (setq history-history-max 256
;;                   history-window-local-history nil)
;;             (history-mode 1)

;;             (bind-key "s-h" 'history-preview-prev-history history-map)
;;             (bind-key "s-n" 'history-preview-next-history history-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smoother scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smooth-scrolling
  :disabled t
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick jumping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-jump-mode
  :commands ace-jump-mode
  :config (progn
            (ace-jump-mode-enable-mark-sync)
            ;; * Without prefix, ace-jump directs toward char
            ;; * With 1 prefix, line
            ;; * With 2 prefixes, word
            (setq ace-jump-mode-submode-list
                  '(ace-jump-char-mode
                    ace-jump-line-mode
                    ace-jump-word-mode))))


;; ace-jump with buffer switching
(use-package ace-jump-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Remembering & jumping back and forth between window layouts
;; (configurations).  Ref: https://www.emacswiki.org/emacs/WinnerMode
;;
(use-package winner
  :init (progn
          (winner-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-compile .el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://www.emacswiki.org/emacs/AutoAsyncByteCompile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (byte-recompile-directory (~get-config) 0)

;; (use-package auto-async-byte-compile
;;   :ensure t
;;   :disabled t
;;   :init (progn
;;           (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;           ;; Don't display buffer after compilation is completed
;;           (setq auto-async-byte-compile-display-function #'identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pop in/pop out shell buffer easily
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/kyagi/shell-pop-el

(use-package shell-pop
  :ensure t
  :config (progn
            (custom-set-variables
             '(shell-pop-universal-key "<S-f9>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation with ease
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package windmove
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quickly copy stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/zonuexe/emacs-copyit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package copyit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eshell
  :commands eshell
  :init (progn
          (use-package exec-path-from-shell
            :ensure t)

          ;; ElDoc in Eshell
          (defadvice eldoc-current-symbol
              (around eldoc-current-symbol activate)
            ad-do-it
            (if (and (not ad-return-value)
                     (eq major-mode 'eshell-mode))
                (save-excursion
                  (goto-char eshell-last-output-end)
                  (let ((esym (eshell-find-alias-function (current-word)))
                        (sym (intern-soft (current-word))))
                    (setq ad-return-value (or esym sym)))))))
  :config (progn
            (add-hook 'eshell-mode-hook 'eldoc-mode)

            (setq eshell-prefer-lisp-functions t)

            ;; Eshell smart mode
            (require 'em-smart)
            (setq eshell-where-to-jump 'begin)
            (setq eshell-review-quick-commands nil)
            (setq eshell-smart-space-goes-to-end t)

            (setq eshell-aliases-file (~get-config "misc/eshell-aliases.el"))

            ;; Beginning of command line, not line
            (defun eshell-beginning-of-command-line ()
              "Move to beginning of command line, not line."
              (interactive)
              (let ((p (point)))
                (beginning-of-line)
                (loop do (forward-char)
                      until (equal (current-char) " "))
                (forward-char)))

            ;; Auto complete support
            (defun ac-pcomplete ()
              ;; eshell uses `insert-and-inherit' to insert a \t if no completion
              ;; can be found, but this must not happen as auto-complete source
              (flet ((insert-and-inherit (&rest args)))
                ;; this code is stolen from `pcomplete' in pcomplete.el
                (let* (tramp-mode ;; do not automatically complete remote stuff
                       (pcomplete-stub)
                       (pcomplete-show-list t) ;; inhibit patterns like * being deleted
                       pcomplete-seen pcomplete-norm-func
                       pcomplete-args pcomplete-last pcomplete-index
                       (pcomplete-autolist pcomplete-autolist)
                       (pcomplete-suffix-list pcomplete-suffix-list)
                       (candidates (pcomplete-completions))
                       (beg (pcomplete-begin))
                       ;; note, buffer text and completion argument may be
                       ;; different because the buffer text may bet transformed
                       ;; before being completed (e.g. variables like $HOME may be
                       ;; expanded)
                       (buftext (buffer-substring beg (point)))
                       (arg (nth pcomplete-index pcomplete-args)))
                  ;; we auto-complete only if the stub is non-empty and matches
                  ;; the end of the buffer text
                  (when (and (not (zerop (length pcomplete-stub)))
                             (or (string= pcomplete-stub ; Emacs 23
                                          (substring buftext
                                                     (max 0
                                                          (- (length buftext)
                                                             (length pcomplete-stub)))))
                                 (string= pcomplete-stub ; Emacs 24
                                          (substring arg
                                                     (max 0
                                                          (- (length arg)
                                                             (length pcomplete-stub)))))))
                    ;; Collect all possible completions for the stub. Note that
                    ;; `candidates` may be a function, that's why we use
                    ;; `all-completions`.
                    (let* ((cnds (all-completions pcomplete-stub candidates))
                           (bnds (completion-boundaries pcomplete-stub
                                                        candidates
                                                        nil
                                                        ""))
                           (skip (- (length pcomplete-stub) (car bnds))))
                      ;; We replace the stub at the beginning of each candidate by
                      ;; the real buffer content.
                      (mapcar #'(lambda (cand) (concat buftext (substring cand skip)))
                              cnds))))))

            (defvar ac-source-pcomplete
              '((candidates . ac-pcomplete)))

            (eval-after-load 'auto-complete
              '(progn
                 (add-to-list 'ac-modes 'eshell-mode)))

            (add-hook 'eshell-mode-hook
                      (lambda ()
                        (setq ac-sources '(ac-source-pcomplete))

                        (add-to-list 'eshell-visual-commands "mc")
                        (add-to-list 'eshell-visual-commands "ranger")
                        (add-to-list 'eshell-visual-commands "git log")
                        ;; (bind-key "s-d" 'eshell-beginning-of-command-line eshell-mode-map)
                        (bind-key "s-d" 'eshell-bol            eshell-mode-map)
                        (bind-key "s-C" 'eshell-previous-input eshell-mode-map)
                        (bind-key "s-T" 'eshell-next-input     eshell-mode-map)
                        (bind-key "<S-mouse-1>" '~insert-text-at-the-end
                                  eshell-mode-map)))

            (setq eshell-prompt-function
                  (lambda ()
                    (let* ((username (getenv "USER"))
                           (hostname (getenv "HOST"))
                           (time     (format-time-string "%Y/%m/%d %H:%M"))
                           (pwd      (eshell/pwd)))
                      (concat "--- " time " " username "@" hostname " " pwd " ---"
                              "\n"
                              "$ "))))

            (setq eshell-prompt-regexp "^[#$] ")

            ;; Read $PATH variable
            (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'sh-mode-hook (lambda ()
                          (setq indent-tabs-mode t
                                sh-basic-offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ~cider-eval (&optional expr)
  "Evals an expression in Cider REPL."
  (interactive)
  (let ((expr (cond ((not (~string-empty? expr))
                     expr)
                    (t
                     (~read-string "Expression: "))))
        (cider-buffer (thread-last (buffer-list)
                        (-filter (lambda (buffer)
                                   (string-match-p "\\*cider-repl.*"
                                                   (buffer-name buffer))))
                        first)))
    (unless (null cider-buffer)
      (set-buffer cider-buffer)
      (goto-char (point-max))
      (insert expr)
      (cider-repl-return)
      (~popup-buffer (buffer-name cider-buffer)))))

(defun ~cider-compile-file-and-run-main ()
  "Compile current file with Cider and run the `-main' function."
  (interactive)
  (call-interactively 'cider-load-current-buffer)
  (~cider-eval "(-main)"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :config (progn
            (~auto-load-mode '("\\.py$") 'python-mode)

            (use-package virtualenvwrapper
              :config (progn
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell)
                        (setq venv-location (or (getenv "WORKON_HOME")
                                                "/m/virtual-envs/"))))

            ;; Doesn't work
            ;; (defun electric-indent-ignore-python (char)
            ;;   "Ignore electric indentation for python-mode"
            ;;   (if (equal major-mode 'python-mode)
            ;;    'no-indent
            ;;  nil))
            ;; (add-hook 'electric-indent-functions 'electric-indent-ignore-python)

            ;; https://www.emacswiki.org/emacs/IndentingPython
            (add-hook 'python-mode-hook
                      (lambda ()
                        (setq electric-indent-chars (delq ?, electric-indent-chars))
                        (setq electric-indent-inhibit t)
                        (~bind-key-with-prefix "e r" 'python-shell-send-region
                                               :keymap python-mode-map)
                        ;; (~bind-key-with-prefix "." 'my/python-jump-to-definition
                        ;;                     :keymap python-mode-map)
                        ;; (~bind-key-with-prefix "," 'my/python-jump-back
                        ;;                     :keymap python-mode-map)
                        ))))

;;
;; Company mode support in Jedi is weak
;;

;; Use autocomplete mode
;; (use-package jedi
;;   :ensure t
;;   :config (progn
;;             (setq jedi:setup-keys nil)
;;             (setq jedi:tooltip-method nil)
;;             (setq jedi:complete-on-dot t)

;;             (autoload 'jedi:setup "jedi" nil t)
;;             (add-hook 'python-mode-hook 'jedi:setup)

;;             (defvar jedi:goto-stack '())

;;             (defun jedi:jump-to-definition ()
;;               (interactive)
;;               (add-to-list 'jedi:goto-stack
;;                            (list (buffer-name) (point)))
;;               (jedi:goto-definition))

;;             (defun jedi:jump-back ()
;;               (interactive)
;;               (let ((p (pop jedi:goto-stack)))
;;                 (if p (progn
;;                         (switch-to-buffer (nth 0 p))
;;                         (goto-char (nth 1 p))))))

;;             (defun my/python-mode-hook ()
;;               (add-to-list 'company-backends 'company-jedi))

;;             (add-hook 'python-mode-hook 'my/python-mode-hook)

;;             ;; (add-hook 'python-mode-hook
;;             ;;           '(lambda ()
;;             ;;              (local-set-key (kbd "C-.") 'jedi:jump-to-definition)
;;             ;;              (local-set-key (kbd "C-,") 'jedi:jump-back)
;;             ;;              (local-set-key (kbd "C-c d") 'jedi:show-doc)
;;             ;;              (local-set-key (kbd "C-<tab>") 'jedi:complete)))
;;             ))

;; Use company-mode
;; (use-package company-jedi
;;   :ensure t
;;   :config (progn
;;             (defvar jedi:goto-stack '())

;;             (defun jedi:jump-to-definition ()
;;               (interactive)
;;               (add-to-list 'jedi:goto-stack
;;                            (list (buffer-name) (point)))
;;               (jedi:goto-definition))

;;             (defun jedi:jump-back ()
;;               (interactive)
;;               (let ((p (pop jedi:goto-stack)))
;;                 (if p (progn
;;                         (switch-to-buffer (nth 0 p))
;;                         (goto-char (nth 1 p))))))

;;             (defun my/python-mode-hook ()
;;               (add-to-list 'company-backends 'company-jedi))

;;             (add-hook 'python-mode-hook 'my/python-mode-hook)))

;;
;; Elpy seems to be a very strong contender
;;
;; http://elpy.readthedocs.io/en/latest/index.html
;; Config with (elpy-config)
;;
;; Setup workflow:
;; * Open a file in the project
;; * Run (pyvenv-workon) and choose the appropriate virtual env
;; * Run (elpy-config) and install necessary dependencies
;;
;; Beginning to work:
;; * Run (pyvenv-workon)
;; * Have fun
;;

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :ensure elpy
  :init (progn
          (elpy-enable)

          ;; I don't want to highlight indentation
          (setq elpy-modules (remove 'elpy-module-highlight-indentation
                                     elpy-modules))

          (setq elpy-rpc-backend "jedi")

          (defvar my/python-goto-stack (list))

          (defun my/python-jump-to-definition ()
            (interactive)
            (add-to-list 'my/python-goto-stack
                         (list (buffer-name) (point)))
            (elpy-goto-definition))

          (defun my/python-jump-back ()
            (interactive)
            (let ((p (pop my/python-goto-stack)))
              (if p (progn
                      (switch-to-buffer (nth 0 p))
                      (goto-char (nth 1 p))))))

          ;; (defun my/elpy-mode-hook ()
          ;;   (bind-key "C-c ." 'my/python-jump-to-definition)
          ;;   (bind-key "C-c ," 'my/python-jump-back))

          ;; (add-hook 'python-mode-hook 'my/elpy-mode-hook)
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package rsense
;;   :init (progn
;;           (setq rsense-home (getenv "$RSENSE_HOME"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode2
  :ensure t
  :commands scala-mode2
  :init
  (progn
    (use-package sbt-mode
      :ensure sbt-mode
      :config
      (progn
        (add-hook 'sbt-mode-hook
                  (lambda ()
                    ;; compilation-skip-threshold tells the compilation minor-mode
                    ;; which type of compiler output can be skipped. 1 = skip info
                    ;; 2 = skip info and warnings.
                    (setq compilation-skip-threshold 1)

                    (bind-key "C-a" 'comint-bol sbt-mode-map)
                    (bind-key "s-d" 'comint-bol sbt-mode-map)
                    (bind-key "M-RET" 'comint-accumulate sbt-mode-map)))
        (add-hook 'scala-mode-hook
                  (lambda ()
                    (bind-key "M-." 'sbt-find-definitions scala-mode-map)
                    (bind-key "C-x '" 'sbt-run-previous-command scala-mode-map)))))))

;;
;; Scheme development
;;

;; Quack doc: http://www.neilvandyke.org/quack/quack.el
;; Geiser doc: http://www.nongnu.org/geiser

;; (use-package geiser
;;   :disabled t
;;   :config (progn
;;             (setq geiser-default-implementation "racket")
;;             (add-hook 'geiser-repl-mode-hook   '~load-paredit-mode)

;;             ;; Auto-complete backend
;;             (use-package ac-geiser
;;               :init (progn
;;                       (add-hook 'geiser-mode-hook        'ac-geiser-setup)
;;                       (add-hook 'geiser-repl-mode-hook   'ac-geiser-setup)
;;                       (eval-after-load 'auto-complete
;;                         '(add-to-list 'ac-modes 'geiser-repl-mode))))

;;             (eval-after-load 'geiser-mode
;;               '(progn
;;                 (dolist (sym '(λ
;;                                ~>
;;                                ~>>
;;                                define-values
;;                                get
;;                                post
;;                                put
;;                                patch
;;                                delete
;;                                call-with-parameterization
;;                                module+))
;;                   (put sym 'scheme-indent-function 1)
;;                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;                 (dolist (sym '(with-shell-commands))
;;                   (put sym 'scheme-indent-function 0)
;;                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;                 (dolist (sym '(module
;;                                module*))
;;                   (put sym 'scheme-indent-function 2)
;;                   (add-to-list 'geiser-racket-extra-keywords (~symbol->string sym)))

;;                 (put '{ 'scheme-indent-function 0)
;;                 (put (~string->symbol "[") 'scheme-indent-function 0)

;;                 (defadvice geiser-eval-region (after send-region-to (&rest arg))
;;                  ;; ad-do-it
;;                  (let ((start (ad-get-arg 0))
;;                        (end   (ad-get-arg 1)))
;;                    (~geiser-send-string (~get-text start end))))

;;                 ;; (ad-deactivate 'geiser-eval-region)
;;                 (ad-activate 'geiser-eval-region)))))

;; Load after Geiser
;; (use-package quack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No longer used
;; (use-package grizzl-read
;;   :config (progn
;;             (bind-key "s-c" 'grizzl-set-selection+1 *grizzl-keymap*)
;;             (bind-key "s-t" 'grizzl-set-selection-1 *grizzl-keymap*)))

(with-eval-after-load "helm"
  (bind-key "s-t" 'helm-next-line helm-map)
  (bind-key "s-c" 'helm-previous-line helm-map)
  (bind-key "M-s-t" '~helm-next-line+ helm-map)
  (bind-key "M-s-c" '~helm-previous-line+ helm-map))

(with-eval-after-load "swoop"
  (bind-key "s-t" 'swoop-action-goto-line-next swoop-map)
  (bind-key "s-c" 'swoop-action-goto-line-prev swoop-map))

;; (bind-key "<f11>" 'helm-show-kill-ring)
;; (bind-key "<f7>" 'project-explorer)
;; (bind-key "<f7>" 'neotree-toggle)
;; (bind-key "<M-f11>" 'helm-all-mark-rings)

;; (bind-key "s-\\" 'helm-semantic-or-imenu)

(bind-key "s-B" '~switch-to-last-buffer)
;; (bind-key "<S-f8>" 'helm-bookmarks)
;; (bind-key "<f8>" 'helm-mini)
(bind-key "<f8>" 'ivy-switch-buffer)
(bind-key "<S-f8>" 'counsel-bookmark)

(eval-after-load 'icicles-cmd1
  '(progn
     ;; S-f4 is always mapped to delete-window
     (global-set-key [remap icicle-kmacro] '~delete-window)))

;; helm-do-ag: Helm'ing while typing to search right away
;; helm-ag: search first, then Helm'ing while typing to filter
;; (bind-key "<f10>" 'helm-do-grep-ag)
;; (bind-key "<f10>" 'helm-ag)
;; (bind-key "<f10>" 'helm-ag-project-root)
;; (bind-key "<f10>" 'helm-do-ag-project-root)
;; (bind-key "<C-f10>" 'helm-do-ag)
;; (bind-key "<M-f10>" 'helm-resume)
;; (bind-key "<C-f10>" 'ack)

;; Fix open-file command in Xiki

(with-eval-after-load "el4r"
  (bind-key "C-o" '~open-line))

;; (~bind-key-with-prefix "\\" 'helm-semantic-or-imenu)
(~bind-key-with-prefix "w h" 'winner-undo)
(~bind-key-with-prefix "w n" 'winner-redo)
 
