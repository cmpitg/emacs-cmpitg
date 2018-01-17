;; -*- no-byte-compile: t; lexical-binding: t; -*-

;;
;; Copyright (C) 2017-2018 Ha-Duong Nguyen (@cmpitg)
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
(el-get 'sync)

(el-get-install 'acme-mouse)

(dolist (package '(subatomic256-theme
                   afternoon-theme
                   monokai-theme
                   solarized-theme
                   autumn-light-theme
                   tango-plus-theme
                   tommyh-theme
                   plan9-theme
                   ht
                   dash
                   which-key
                   save-visited-files
                   paredit
                   projectile
                   point-pos
                   expand-region
                   dumb-jump
                   embrace
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
                   visual-fill-column
                   company
                   company-quickhelp
                   pos-tip
                   vimdiff
                   epa
                   dired+
                   dired-single
                   dired-details+
                   color-theme
                   yasnippet
                   dockerfile-mode
                   nginx-mode
                   coffee-mode
                   go-mode
                   slime
                   slime-company
                   erlang
                   edts-start
                   elixir-mode
                   alchemist
                   gradle-mode
                   groovy-mode
                   jdee
                   clojure-mode
                   clojure-cheatsheet
                   clojurescript-mode
                   midje-mode
                   clj-refactor
                   cider
                   scss-mode
                   js2-mode
                   ruby-mode
                   ruby-dev
                   toml-mode
                   haskell-mode
                   request
                   elpy
                   monky
                   magit))
  (package-install package))
