;;
;; Copyright (C) 2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
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

(defun ~setup-moz-javascript ()
  "Setting JavaScript mode with MozRepl."
  (moz-minor-mode 1))

(defun ~sunrise ()
  "Open Sunrise Commander, remove the nonpane buffer."
  (interactive)
  (unless sr-running
    (sunrise)
    (sr-reset-view-remove-nonpane-buffer)))

(defun ~sunrise-cd ()
  "Open Sunrise Commander with current directory, remove the
nonpage buffer."
  (interactive)
  (unless sr-running
    (sunrise-cd)
    (sr-reset-view-remove-nonpane-buffer)))

(defun sr-reset-view ()
  "Reset Sunrise Commander pane view."
  (interactive)
  (when sr-running
    (sr-setup-windows)))

(defun sr-reset-view-remove-nonpane-buffer ()
  "Reset Sunrise Commander pane view, removing the nonpane
buffer."
  (interactive)
  (when sr-running
    (sr-setup-windows)
    (windmove-down)
    (delete-window)))

(defun ~auto-load-mode (filetypes mode)
  "Autoload mode for filetype regex or a list of filetypes.

Example:

  \(~auto-load-mode \"\\\\.rake$\" 'ruby-mode\)
  \(~auto-load-mode '(\"\\\\.md$\" \"\\\\.markdown$\") 'markdown-mode\)
"
  (if (stringp filetypes)
    (add-to-list 'auto-mode-alist (cons filetypes mode))
    (dolist (filetype filetypes)
      (add-to-list 'auto-mode-alist (cons filetype mode)))))

(defun ~noweb-code-chunk-add-mode (mode-name)
  "Add mode to a code chunk"
  (interactive "MMode name (without `-mode`): ")
  (insert (concat "-*- mode: " mode-name " -*-")))

(defun ~compile-haml ()
  "Compile HAML file to HTML file."
  (interactive)
  (and (string-match ".*\.haml$" (~current-file-full-path))
       (let ((output-file (replace-regexp-in-string "\.haml$" ".html"
                                                    (~current-file-full-path))))
         (compile (concat "haml \"" (~current-file-full-path) "\" "
                          "\"" output-file "\"")))))

(defun ~compile-coffee ()
  "Compile CoffeeScript to JavaScript."
  (interactive)
  (and (string-match ".*\.coffee$" (~current-file-full-path))
       (compile (concat "coffee -c " (~current-file-full-path)))))

(defun ~compile-livescript ()
  "Compile LiveScript to JavaScript."
  (interactive)
  (and (string-match ".*\.ls$" (~current-file-full-path))
       (compile (concat "livescript -c -d " (~current-file-full-path)))))

(defun ~toggle-ibus ()
  "Toggle ibus."
  (interactive)

  (unless (~is-var-defined? '*is-ibus-on?*)
    (defvar *is-ibus-on?* nil))

  (if (null *is-ibus-on?*)
    (progn (ibus-enable)
           (setf *is-ibus-on?* t))
    (progn (ibus-disable)
           (setf *is-ibus-on?* nil))))

(defun ~toggle-ecb ()
  "Toggle ECB."
  (interactive)

  (unless (~is-var-defined? '*is-ecb-running?*)
    (defvar *is-ecb-running?* nil))

  (if (null *is-ecb-running?*)
    (progn (ecb-activate)
           (setf *is-ecb-running?* t))
    (progn (ecb-deactivate)
           (setf *is-ecb-running?* nil))))

(defun ~load-paredit-mode ()
  "Load paredit mode and disable autopair."
  (paredit-mode t)
  (when (~is-function-defined? 'autopair-mode)
    (autopair-mode 0))
  (when (~is-function-defined? 'smartparens-mode)
    (smartparens-mode 0)))

(defun ~markdown-outline-headings ()
  "Outline headings for current Markdown document using Helm."
  (interactive)
  (let ((helm-multi-occur-buffer-list (list (buffer-name (current-buffer)))))
    (helm-occur-init-source)
    (helm :input "\\#"
          :sources 'helm-source-occur
          :buffer "*helm occur*"
          :history 'helm-grep-history
          :truncate-lines t)))

(defun ~markdown-make-inline-link ()
  "Turn `text1|text2` to Markdown's inline link format:
`[text1](text2)`."
  (interactive)
  (let* ((text (get-selection))
         (parts (s-split "|" text))
         (name     (first parts))
         (link-to  (s-join "|" (rest parts))))
    (replace-selection  (format "[%s](%s)" name link-to))))

(defalias 'setup-moz-javascript '~setup-moz-javascript)
(defalias 'auto-load-mode '~auto-load-mode)
(defalias 'noweb-code-chunk-add-mode '~noweb-code-chunk-add-mode)
(defalias 'compile-livescript '~compile-livescript)
(defalias 'compile-coffee '~compile-coffee)
(defalias 'compile-haml '~compile-haml)
(defalias 'toggle-ibus '~toggle-ibus)
(defalias 'toggle-ecb '~toggle-ecb)
(defalias 'load-paredit-mode '~load-paredit-mode)
(defalias 'markdown-make-inline-link '~markdown-make-inline-link)
(defalias 'markdown-outline-headings '~markdown-outline-headings)
