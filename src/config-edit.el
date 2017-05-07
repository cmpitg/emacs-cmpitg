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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode)

    (add-to-list 'projectile-globally-ignored-directories "node_modules*")
    (add-to-list 'projectile-globally-ignored-directories "*bower_components*")
    (add-to-list 'projectile-globally-ignored-directories "bower_components")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-files "*.pyc")
    (add-to-list 'projectile-globally-ignored-files ".*pyc")

    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-find-dir-includes-top-level t)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)

    ;; (setq projectile-switch-project-action 'projectile-dired)
    ;; (setq projectile-switch-project-action 'projectile-find-dir)

    ;; Customize find file command via function
    ;; projectile-get-ext-command
    (setq projectile-git-command projectile-generic-command)
    (setq projectile-hg-command projectile-generic-command)))

;; Integration with Helm completion framework
(use-package helm-projectile
  :ensure t
  :config
  (progn
    (helm-projectile-on)
    (setq projectile-completion-system 'helm)
    (setq projectile-require-project-root t)
    (setq projectile-switch-project-action 'projectile-dired)

    ;; A bug in projectile ignore that doesn't ignore
    (setq projectile-indexing-method 'native)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file processing with Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tramp)

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
;; Support for tar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tar-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open with external program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load from local
;;
;; Seems to be abandoned.  We'll find another way to provide the openwith
;; functionality.
;;

;; (use-package openwith
;;   :config (progn
;;             (openwith-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asciidoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package adoc-mode
  :ensure t
  :defer t
  :commands adoc-mode
  :mode ("\\.adoc\\'" . adoc-mode))

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

;;
;; Use for one-off searches (as the Helm buffer is not persistent)
;;
(use-package helm-ag
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m web browser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package w3m
  :ensure t
  :commands w3m-browse-url
  :init (progn
          (setq browse-url-browser-function 'w3m-browse-url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better menu bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package menu-bar+
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (progn
            ;; (use-package markdown-mode+
            ;;   :ensure markdown-mode+)

            (custom-set-faces
             ;; Your init file should contain only one such instance.
             ;; If there is more than one, they won't work right.
             '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7 :background "#ABCDEF"))))
             '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5 :background "green"))))
             '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; En/decoding JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(el-get-install 'json-mode)

(use-package json-mode
  :defer t
  :mode "\\.json\\'"
  :init (progn
          (eval-after-load "json-mode"
            '(progn
              (setq c-basic-offset 2)
              (setq tab-width 2)))))

(use-package json
  :ensure t
  :defer t)

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
;; Neotree might break tab completion in minibuffer
;;
;; https://github.com/jaypei/emacs-neotree
;; https://www.emacswiki.org/emacs/NeoTree
;;

(use-package neotree
  :ensure t
  :config
  (progn
    ;; (setq neo-theme 'icons)
    (setq neo-theme 'arrow)

    (add-hook 'neotree-mode-hook
              (lambda ()
                (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
                (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
                (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
                (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)))

    ;; If you use popwin, when NeoTree is open and successively a temporary
    ;; buffer is opened with popwin, a new window with the NeoTree buffer is
    ;; displayed side by side next to the first one (#50). This code will help
    ;; you

    ;; (when neo-persist-show
    ;;   (add-hook 'popwin:before-popup-hook
    ;;             (lambda () (setq neo-persist-show nil)))
    ;;   (add-hook 'popwin:after-popup-hook
    ;;             (lambda () (setq neo-persist-show t))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package recentf
  :ensure t
  :init (progn
          (recentf-mode 1)
          (setq recentf-max-menu-items 128)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading functionalities for editting")
(provide 'ee:config-edit)
