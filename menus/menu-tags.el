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
