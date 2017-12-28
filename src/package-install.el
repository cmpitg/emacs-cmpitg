(defun package-install:get-config (&rest paths)
  "Returns path to a config file or directory."
  (let ((config-dir (file-name-directory (or (buffer-file-name)
                                             load-file-name))))
    (apply 'concat *config-dir* paths)))

(require 'ee:config-package-manager (~get-config "config-package-manager"))
(el-get 'sync)

(el-get-install 'acme-mouse)

(dolist (package '(which-key
                   point-pos
                   flx
                   hydra
                   counsel
                   ivy
                   ivy-hydra
                   imenu-anywhere
                   async
                   timp))
  (package-install package))
