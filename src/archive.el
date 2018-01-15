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
