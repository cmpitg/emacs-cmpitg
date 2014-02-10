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

;; Defining `Tags` menu

(make-submenu [menu-bar tags]
              :title "Tags")

(make-menu-item [menu-bar tags create-tags]
                :title "Create tags"
                :tooltip "Create a tag table from current directory."
                :action '$create-tags)

(make-menu-item [menu-bar tags list-tags]
                :title "List tags"
                :action 'helm-etags-select)

(make-menu-item [menu-bar tags goto-tag]
                :title "Go to tag"
                :action 'goto-tag
                :tooltip "Jump to declaration of a tag")

(make-menu-item [menu-bar tags goto-tag-other-window]
                :title "Go to tag (other window)"
                :action 'goto-tag-other-window
                :tooltip "jump to declaration of a tag in another window")

;; (build-menu
;;  '([menu-bar tags] "Tags"
;;    ([create-tags]  "Create tags" "Create a tag table from current directory")
;;    ([list-tags]    "List tags")
;;    ([goto-tag]     "Go to tag" "Jump to declaration of a tag")))

;; (build-menu-action
;;  '([menu-bar tags]
;;    ([create-tags]  'create-tags)
;;    ([list-tags]    'helm-etags-select)
;;    ([goto-tag]     'goto-tag)))
