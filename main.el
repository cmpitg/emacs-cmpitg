;;
;; Copyright (C) 2012-2013 Duong H. Nguyen <cmpitgATgmaildotcom>
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

;;;
;;; TODO
;;;
;;; * Add function to lookup English (Oxford Advanced and Merriam-Webster)
;;;   and Spanish words.
;;; * This file actually install packages and config everything, rename this
;;;   to reflect the task
;;; * write documentation/make screencast about workflow
;;; * Discover Helm menu
;;; * Discover construct regexp menu and make screencast
;;; * Discover and write about `auto-insert`
;;; * helm-locate-library
;;; * DONE Rename this file
;;; * DONE Global variables are placed in a seperated file
;;; * DONE Load everything with `eval-after-load` so config would be messed up if a
;;;   package is not loaded
;;; * Better interface to ELPA and el-get:
;;;   - DONE Check if a package exists
;;;   - Check for package dependencies
;;;   - Update package DB
;;;   - Automatically check for updates
;;;   - Update packages
;;; * DONE [task] Decouple: defining & implementing (global vars at one place)
;;; * Function: better M-q inside comments
;;; * Modularize all of my customization:
;;;   - Each feature is self-documented, using TomDoc, with HTML output hosted
;;;     on my site
;;;   - (require 'a-feature) to load
;;; * [doc] Documentation and cheatsheet for all custom functions, with HTML
;;;   output for online browsing and inline lookup
;;; * [doc] Guide on how to package
;;; * [doc] Guide on how to write a mode
;;; * [task] Structural editing (suggestion based on the context, at a
;;;   separated window, mouse click support, easy manipulation)
;;; * Sublime-like preview buffer

;; TODO
;;     (helm
;;      :sources '(helm-source-grep)
;;      :buffer (format "*helm %s*" (if zgrep "zgrep" (helm-grep-command recurse)))
;;      :default-directory helm-grep-last-default-directory
;;      :keymap helm-grep-map ; [1]
;;      :history 'helm-grep-history
;;      :truncate-lines t)))
;;
