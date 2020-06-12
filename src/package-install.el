;; -*- no-byte-compile: t; lexical-binding: t; -*-

;;
;; Copyright (C) 2017-2020 Ha-Duong Nguyen (@cmpitg)
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

(defun package-install:get-config (&rest paths)
  "Returns path to a config file or directory."
  (let ((config-dir (file-name-directory (or (buffer-file-name)
                                             load-file-name))))
    (apply 'concat *config-dir* paths)))

(require 'rmacs:config-package-manager
         (package-install:get-config "config-package-manager"))

(package-refresh-contents)

;; (el-get-install 'acme-mouse)
;; (el-get-install 'mu4e)

(dolist (package '(auto-compile
                   subatomic256-theme
                   afternoon-theme
                   monokai-theme
                   solarized-theme
                   autumn-light-theme
                   tango-plus-theme
                   tommyh-theme
                   plan9-theme
                   ibuffer-sidebar
                   diminish
                   use-package
                   lua-mode
                   ht
                   dash
                   which-key
                   save-visited-files
                   paredit
                   projectile
                   point-pos
                   expand-region
                   dumb-jump
                   vdiff
                   ;; embrace
                   flx
                   hydra
                   amx
                   counsel
                   ivy
                   ivy-hydra
                   counsel
                   imenu-anywhere
                   async
                   timp
                   guix
                   ;; ace-window
                   ace-jump-buffer
                   ace-jump-mode
                   multiple-cursors
                   yasnippet
                   smartparens
                   dtrt-indent
                   ;; emamux
                   chruby
                   exec-path-from-shell
                   evil
                   evil-visualstar
                   evil-collection
                   rust-mode
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
                   visual-fill-column
                   company
                   company-quickhelp
                   pos-tip
                   vdiff
                   epa
                   dired+
                   dired-single
                   dired-details+
                   color-theme
                   dockerfile-mode
                   nginx-mode
                   coffee-mode
                   go-mode
                   sly
                   ;; slime
                   ;; slime-company
                   erlang
                   edts
                   elixir-mode
                   alchemist
                   gradle-mode
                   groovy-mode
                   clojure-mode
                   clojurescript-mode
                   flycheck-clj-kondo
                   midje-mode
                   clj-refactor
                   cider
                   scss-mode
                   js2-mode
                   tide
                   add-node-modules-path
                   zig-mode
                   ruby-mode
                   ruby-dev
                   toml-mode
                   haskell-mode
                   request
                   conda
                   elpy
                   monky
                   magit
                   evil-magit
                   elpa-mirror
                   resize-window
                   wgrep
                   switch-buffer-functions
                   iflipb
                   sublimity
                   smooth-scrolling
                   rainbow-mode))
  (unless (package-installed-p package)
    (package-install package)))
