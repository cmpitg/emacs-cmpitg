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
