;; -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;; Copyright (C) 2026 Ha-Duong Nguyen (@cmpitg)
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

(with-eval-after-load "transient"
  (transient-define-prefix ~menu/main ()
    "Top-level menu, entrypoint for (almost) everything."
    [["Conveniences"
      ("SPC"     "Mark sexp"        mark-sexp            :transient t)
      ("u"       "Tree undo"        ~tree-undo)
      ("="       "Text scale"       text-scale-adjust)
      ("m"       "Region"           ~menu/region)
      (","       "Pop mark"         pop-to-mark-command  :transient t)
      ("<prior>" "To the beginning" beginning-of-buffer)
      ("<next>"  "To the end"       end-of-buffer)
      ]
     ["File"
      ("b" "Buffer"  ~menu/buffer)
      ("w" "Window"  ~menu/window)
      ("r" "Frame"   ~menu/frame)
      ("f" "File"    ~menu/file)
      ("j" "Jumping" ~menu/jumping)
      ]
     ["Editing"
      ("d"   "Edit"               ~menu/edit)
      ("e"   "Structural editing" ~menu/structural-edit)
      ("o"   "Org"                ~menu/org)
      ("i"   "Insert"             ~menu/insert)
      ]
     ["Commands"
      ("<f7>" "M-x"        ~interactively-M-x)
      ("l"    "Emacs Lisp" ~menu/emacs-lisp)
      ("n"    "Mode"       ~menu/mode)
      ]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/org ()
    "Org"
    [["Navigation"
      ("j" "Next visible heading"     org-next-visible-heading        :transient t)
      ("k" "Prev visible heading"     org-previous-visible-heading    :transient t)
      ("J" "Next heading, same level" org-forward-heading-same-level  :transient t)
      ("K" "Prev heading, same level" org-backward-heading-same-level :transient t)
      ("/" "Sparse tree"              org-sparse-tree)
      ("g g" "Go to"                  org-goto)
      ]
     ["Action"
      ("a"   "Archive subtree"    org-archive-subtree   :transient t)
      ("c"   "Set tags or update" org-ctrl-c-ctrl-c     :transient t)
      (">"   "Demote subtree"     org-shiftmetaright    :transient t)
      ("<"   "Promote subtree"    org-shiftmetaleft     :transient t)
      ("]"   "Demote heading"     org-metaright         :transient t)
      ("["   "Promote heading"    org-metaleft          :transient t)
      ("m"   "Mark subtree"       org-mark-subtree      :transient t)
      ("P"   "Move subtree up"    org-move-subtree-up   :transient t)
      ("N"   "Mark subtree down"  org-move-subtree-down :transient t)
      ("TAB" "Cycle"              org-cycle             :transient t)]
     ["Literate programming"
      ("t t" "Tangle"        org-babel-tangle)
      ("t g" "Go to block"   org-babel-goto-named-src-block)
      ("t e" "Edit"          org-edit-src-code)
      ("t s" "Save editing"  org-edit-src-save)
      ("t q" "Exit editing"  org-edit-src-exit)
      ("t k" "Abort editing" org-edit-src-abort)
      ]
     ["Insert"
      ("i i" "Heading"            org-insert-heading)
      ("i I" "Item"               org-insert-item)
      ("i s" "Structure template" org-insert-structure-template)
      ]
     ["TODO"
      ("o i" "Insert heading" org-insert-todo-heading)
      ("o c" "Change state"   org-todo :transient t)
      ]
     ["Narrowing"
      ("n s" "Narrow to subtree" org-narrow-to-subtree :transient t)
      ("n b" "Narrow to block"   org-narrow-to-block   :transient t)
      ("n w" "Widen"             widen                 :transient t)
      ]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/insert ()
    "Insert ops"
    [["Insert"
      ("e"   "Executable"   ~insert-exec)
      (";"   "Line comment" ~insert-full-line-comment)
      ("t t" "Today"        ~insert-today)
      ("t n" "Now"          ~insert-now)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/jumping ()
    "Jumping ops"
    [["Jumping"
      ("." "Dumb-jump go"      ~dumb-jump-go)
      ("," "Dumb-jump go back" ~dumb-jump-go-back :transient t)
      ("s" "Save point"        ~point-save)
      ("g" "Go to saved point" ~point-goto)
      ("n" "Go to next point"  ~point-next        :transient t)
      ("p" "Go to prev point"  ~point-prev        :transient t)
      ("i" "Symbol menu"       ~interactively-call-symbol-menu)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/emacs-lisp ()
    "Emacs Lisp ops"
    [["Eval"
      ("e" "Eval last sexp or region" ~eval-last-sexp-or-region)
      ("p" "Eval+pp last sexp"        pp-eval-last-sexp)
      ("x" "Prompt eval"              eval-expression)
      ("r" "Eval region"              eval-region)
      ("b" "Eval buffer"              eval-buffer)
      ("f" "Eval defun"               eval-defun)
      ("c" "Eval then replace"        ~eval-then-replace-region-or-last-sexp)]
     ["Help"
      ("h f" "Describe function" describe-function)
      ("h v" "Describe variable" describe-variable)
      ("h k" "Describe key"      describe-key)
      ("h ." "Find function"     find-function)
      ("h l" "Find library"      find-library)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/mode ()
    "Mode ops"
    [["Buffer"
      ("s" "Toggle whitescape mode" whitespace-mode       :transient t)
      ("f" "Toggle font lock mode"  font-lock-mode        :transient t)
      ("w" "Toggle soft-wrapping"   ~toggle-soft-wrapping :transient t)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/buffer ()
    "Buffer ops"
    [["Buffer"
      ("r" "Revert current"        revert-buffer)
      ("k" "Kill current"          kill-current-buffer  :transient t)
      ("t" "Undo killing"          ~undo-killed-buffers)
      ("m" "Mark current"          mark-whole-buffer)
      ("n" "New"                   ~new-buffer)
      ("l" "Show simple chooser"   ~show-buffer-chooser)
      ("b" "Show advanced chooser" list-buffers)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  ;; TODO URGENT Fix the scroll and the toggle-max functions
  (transient-define-prefix ~menu/window ()
    "Window ops"
    [["Window"
      ("o" "Delete others"                  ~one-window)
      ("s" "Scroll other window"            ~scroll-other-window         :transient t)
      ("S" "Scroll other window (reversed)" ~scroll-other-window-reverse :transient t)
      ("k" "Delete current"                 delete-window                :transient t)
      ("t" "Transpose"                      ~transpose-windows           :transient t)
      ("y" "Toggle stickiness"              ~toggle-sticky-window        :transient t)
      ("z" "Toggle max"                     ~toggle-maximize-buffer      :transient t)
      ("r" "Split right"                    ~split-window-right          :transient t)
      ("b" "Split below"                    ~split-window-below          :transient t)
      ("e" "Resize window"                  resize-window)
      ("w" "Focus another"                  other-window                 :transient t)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/frame ()
    "Frame ops"
    [["Frame"
      ("n" "New frame"  make-frame)
      ("k" "Kill frame" delete-frame)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/file ()
    "File ops"
    [["Current file"
      ("d"   "Delete"          ~delete-current-file)
      ("r"   "Rename"          ~rename-current-file)
      ("s"   "Save"            save-buffer)
      ("a"   "Save as"         ~save-buffer-as)
      ("o"   "Open"            ~interactively-find-file)
      ("f"   "Open in project" ~interactively-find-file-in-project)
      ("m"   "Open as admin"   ~open-current-file-as-admin)
      ("c"   "Copy file name"  ~copy-file-name-to-clipboard)
      ("p"   "Copy position"   ~copy-pos-to-clipboard)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/edit ()
    "Editing ops"
    [["General"
      ("i"   "Indent rigidly"      indent-rigidly)
      ("j"   "Join with next line" ~join-with-next-line        :transient t)
      ("c"   "Un/comment region"   comment-or-uncomment-region :transient t)
      ("d"   "Duplicate"           ~duplicate-line-or-region   :transient t)
      ("z"   "Repeat most recent"  repeat                      :transient t)
      ("t"   "Open term emu"       ~open-term-emu)
      ("e"   "Edit with Helix"     ~edit-with-helix)]
     ["Highlight"
      ("h h" "Thing"              highlight-regexp)
      ("h h" "Line"               highlight-lines-matching-regexp)
      ("h k" "Unhighlight regexp" unhighlight-regexp)]
     ["Aligning"
      ("a a" "Align"         align)
      ("a c" "Align current" align-current)
      ("a r" "Align regexp"  align-regexp)]
     ["Narrowing"
      ("n r" "Narrow to region"     narrow-to-region)
      ("n f" "Narrow to defun"      narrow-to-defun)
      ("n b" "Org: narrow to block" org-narrow-to-block)
      ("n w" "Widen"                widen)]
     ["Whitescape"
      ("w o" "Keep just one space" just-one-space)
      ("w d" "Delete spaces"       delete-horizontal-space)
      ("w w" "Cleanup"             whitespace-cleanup)]
     ["VC"
      ("g s" "Magit: Status" ~git-status)
      ("g b" "Magit: Blame"  ~git-blame)
      ("g d" "Magit: Diff"   ~git-diff)
      ("v a" "VC: Annotate"  vc-annotate)
      ("v d" "VC: Diff"      vc-diff)]
     ["Submenus"
      ("m" "Multiple cursor" ~menu/multiple-cursors)
      ("/" "Search"          ~menu/search)
      ]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/structural-edit ()
    "Structural editing ops"
    [["Parens and tags"
      ("S"       "Split"            puni-split                  :transient t)
      ("s"       "Splice"           puni-splice                 :transient t)
      (">"       "Slurp forward"    puni-slurp-forward          :transient t)
      ("<"       "Slurp backward"   puni-slurp-backward         :transient t)
      ("SPC SPC" "Mark at"          puni-mark-sexp-at-point     :transient t)
      ("SPC a"   "Mark sexp around" puni-mark-sexp-around-point :transient t)
      ("SPC l"   "Mark list around" puni-mark-list-around-point :transient t)]
     ["Submenus"
      ("p" "Smartparens" ~menu/smartparens)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]]
    )

  (transient-define-prefix ~menu/smartparens ())
  (with-eval-after-load "smartparens"
    (transient-define-prefix ~menu/smartparens ()
      "Smartparens editing"
      [["Smartparens"
        (">" "Slurp forward"  sp-forward-slurp-sexp  :transient t)
        ("<" "Slurp backward" sp-backward-slurp-sexp :transient t)
        (")" "Barp forward"   sp-forward-barf-sexp   :transient t)
        ("(" "Barp backward"  sp-backward-barf-sexp  :transient t)
        ("l" "Move forward"   sp-forward-sexp        :transient t)
        ("h" "Move backward"  sp-backward-sexp       :transient t)
        ("j" "Move up"        sp-down-sexp           :transient t)
        ("k" "Move down"      sp-backward-up-sexp    :transient t)
        ("s" "Split"          sp-split-sexp          :transient t)
        ("S" "Splice"         sp-splice-sexp         :transient t)
        ("j" "Join"           sp-join-sexp           :transient t)]
       ["Meta"
        ("C-g" "Quit menu" transient-quit-all)
        ("q"   "Quit menu" transient-quit-all)
        ("`"   "Back"      transient-quit-one)]]))


  ;; TODO RIGHT HERE
  (~defalias-when-load "expand-region" '~expand-region 'er/expand-region)
  (~defalias-when-load "expand-region" '~mark-html-attr 'er/mark-html-attribute)
  (~defalias-when-load "expand-region" '~mark-paragraph 'er/mark-paragraph)
  (~defalias-when-load "expand-region" '~mark-defun 'er/mark-defun)

  (transient-define-prefix ~menu/region ())
  (with-eval-after-load "expand-region"
    ;; (message-box "Loading ~menu/region")
    (setq expand-region-fast-keys-enabled nil)
    (transient-define-prefix ~menu/region ()
      "Region editing ops"
      [["Region"
        ("=" "Expand region"  er/expand-region       :transient t)
        ("a" "Mark HTML attr" er/mark-html-attribute :transient t)
        ("p" "Mark paragraph" er/mark-paragraph      :transient t)
        ("f" "Mark defun"     er/mark-defun          :transient t)
        ("c" "Mark comment"   er/mark-comment        :transient t)
        ("i" "Mark inner tag" er/mark-inner-tag      :transient t)
        ("o" "Mark outer tag" er/mark-outer-tag      :transient t)
        ("h" "Mark tag"       er/mark-nxml-tag       :transient t)
        ]
       ["Meta"
        ("C-g"      "Quit menu" transient-quit-all)
        ("q"        "Quit menu" transient-quit-all)
        ("<escape>" "Back"      transient-quit-one)]]))

  (transient-define-prefix ~menu/search ()
    "Search ops"
    [[("/" "Interactive search"   ~interactively-search)
      ("r" "Query replace"        query-replace)
      ("R" "Query replace regexp" query-replace-regexp)]
     ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]])

  (transient-define-prefix ~menu/multiple-cursors ())
  (with-eval-after-load "multiple-cursors"
    (transient-define-prefix ~menu/multiple-cursors ()
      "Multiple cursors"
      [["Ops"
        ("l" "Edit lines"              mc/edit-lines)
        ("n" "Mark next like this"     mc/mark-next-like-this)
        ("p" "Mark previous like this" mc/mark-previous-like-this)
        ("a" "Mark all in region"      mc/mark-all-in-region)]
       ["Meta"
      ("C-g"      "Quit menu" transient-quit-all)
      ("q"        "Quit menu" transient-quit-all)
      ("<escape>" "Back"      transient-quit-one)]]))
  )
;; TODO RIGHT HERE puni-slurp-forward
;; TODO RIGHT HERE puni-split

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finished loading menus")

(provide 'rmacs:config-menu)
