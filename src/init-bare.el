;; -*- no-byte-compile: t -*-

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

;; Make GC aware that we have a lot of memory
(setq gc-cons-threshold (* 100 1024 1024))

(defconst rmacs:+init-src-dir+ (or (file-name-directory (or load-file-name buffer-file-name))
				   defaulte-directory))

(defconst rmacs:+bootstrap-config-path+
  (file-name-concat rmacs:+init-src-dir+ "bootstrap-config.el")
  "Path to the Rmacs boostrap-config file.")

;; Speed up Emacs startup by ignoring all file name handlers.  Review: Ref:
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(require 'rmacs:bootstrap-config                      rmacs:+bootstrap-config-path+)
(require 'rmacs:config-ipc                            "config-ipc")
(require 'rmacs:bootstrap-functionality               "bootstrap-functionality")
(require 'rmacs:config-module-wand-minimal            "config-module-wand-minimal")
(require 'rmacs:config-module-omni-command            "config-module-omni-command")
(require 'rmacs:config-module-simple-buffer-list      "config-module-simple-buffer-list")
(require 'rmacs:config-header-line                    "config-header-line")
(require 'rmacs:config-module-bowser                  "config-module-bowser")

(message "Finish loading bare functionalities")
