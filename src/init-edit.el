;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2014-2025 Ha-Duong Nguyen (@cmpitg)
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

;; Always starts with bare config
(load (file-name-concat (file-name-directory (or load-file-name
                                                 (buffer-file-name)))
                        "init-bare"))

(require 'rmacs:config-package-manager                "config-package-manager")
(require 'rmacs:config-core-functions                 "config-core-functions")
(require 'rmacs:config-module-org-mode                "config-module-org-mode")
(require 'rmacs:config-core-edit                      "config-core-edit")
(require 'rmacs:config-core-ux                        "config-core-ux")
(require 'rmacs:config-themes                         "config-themes")
(require 'rmacs:config-core-keybindings               "config-core-keybindings")
(require 'rmacs:config-module-convenient-buffer-shell "config-module-convenient-buffer-shell")

(require 'rmacs:config-functions                      "config-functions")
(require 'rmacs:functions-cmpitg                      "functions-cmpitg")
(require 'rmacs:config-edit                           "config-edit")
(require 'rmacs:commands-cmpitg                       "commands-cmpitg")

(unless (string= "1" (getenv "EMACS_NO_EXPERIMENTAL"))
  (~load-files (~get-config "experimental")))

;; Machine/user-specific config
(~load-files "~/.emacs-machine-specific" (~get-config "machine-specific"))

;;
;; Last but not least - remember
;;

;; Make user all actions queued by Elpaca are executed before we enjoy Emacs
(with-eval-after-load "elpaca"
  (elpaca-process-queues))

(require 'rmacs:config-core-last                      "config-core-last")

(message "Finish loading Rmacs edit:%s!" server-name)
(~run-process (message "notify-send %s"
                       (shell-quote-argument (format "Finish loading Rmacs edit:%s!" server-name)))
              :async t)
