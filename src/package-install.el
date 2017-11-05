(defun package-install:get-config (&rest paths)
  "Returns path to a config file or directory."
  (let ((config-dir (file-name-directory (or (buffer-file-name)
                                             load-file-name))))
    (apply 'concat *config-dir* paths)))

(require 'ee:config-package-manager (~get-config "config-package-manager"))

(el-get-install 'acme-mouse)
(package-install 'point-pos)
