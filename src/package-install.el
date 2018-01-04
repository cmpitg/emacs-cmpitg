(defun package-install:get-config (&rest paths)
  "Returns path to a config file or directory."
  (let ((config-dir (file-name-directory (or (buffer-file-name)
                                             load-file-name))))
    (apply 'concat *config-dir* paths)))

(require 'ee:config-package-manager (~get-config "config-package-manager"))
(el-get 'sync)

(el-get-install 'acme-mouse)

(dolist (package '(which-key
                   paredit
                   projectile
                   point-pos
                   expand-region
                   flx
                   hydra
                   counsel
                   ivy
                   ivy-hydra
                   counsel
                   imenu-anywhere
                   async
                   timp
                   ace-window
                   ace-jump-buffer
                   ace-jump-mode
                   point-pos
                   multiple-cursors
                   yasnippet
                   smartparens
                   dtrt-indent
                   emamux
                   evil
                   evil-visualstar
                   evil-paredit
                   evil-surround
                   treemacs
                   treemacs-evil
                   treemacs-projectile
                   menu-bar+
                   w3m
                   adoc-mode
                   wand
                   markdown-mode
                   yaml-mode
                   json-mode
                   json
                   scroll-restore
                   visual-fill-column))
  (package-install package))
