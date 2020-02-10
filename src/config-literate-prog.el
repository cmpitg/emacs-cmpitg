;;
;; Copyright (C) 2014-2018 Ha-Duong Nguyen (@cmpitg)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literate programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: Support local state & multiple narrowing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ht)

;;
;; Enhance narrow to region in ASCIIDoc mode.
;; Doesn't support multiple narrowing, yet.
;;

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defvar *saved-major-mode-for-narrow* (ht))
(defvar *language->mode-symbol* (ht ('racket-mode 'scheme-mode)))
(defvar *narrowed-spaces* nil)

(defun ~widen-narrowed-region ()
  "Widens the narrowed region and reset major mode by saving and
reverting the buffer."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (loop until (eobp)
          do
          (beginning-of-line)
          (unless (~current-line-empty?)
            (insert *narrowed-spaces*))
          (forward-line 1)))
  (setq *narrowed-spaces* nil)          ; Not narrowed anymore
  (widen)
  (call-interactively (ht-get *saved-major-mode-for-narrow* (buffer-file-name)))
  (recenter-top-bottom))

(defun narrow-to-region-indirect (start end)
  "Restricts editing in this buffer to the current region,
indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun ~narrow-to-code-region-markdown ()
  "Narrows to code region in Markdown mode.  Reindents."
  (interactive)
  (save-excursion
    (let* ((begin-code-region (rx (0+ " ")
                                  "```"
                                  (0+ (any alnum "-_/"))
                                  eol))
           (end-code-region (rx (0+ " ")
                                "```"
                                eol))
           (language (save-excursion
                       (re-search-backward begin-code-region)
                       (let* ((line (thing-at-point 'line)))
                         (if (string-match (rx (0+ " ")
                                               "```"
                                               (group (1+ (any alnum "-_/")))
                                               eol)
                                           line)
                             (match-string 1 line)
                           "text"))))
           (begin-point (save-excursion
                          (re-search-backward begin-code-region)
                          (next-line)
                          (beginning-of-line)
                          (point)))
           (n-spaces (save-excursion
                       (re-search-forward begin-code-region)
                       (let* ((line (thing-at-point 'line))
                              (spaces (progn
                                        (string-match (rx bol (group (0+ " ")))
                                                      line)
                                        (match-string 1 line))))
                         (length spaces))))
           (end-point (save-excursion
                        (re-search-forward end-code-region)
                        (previous-line)
                        (end-of-line)
                        (point))))
      (ht-set! *saved-major-mode-for-narrow* (buffer-file-name) major-mode)
      (narrow-to-region begin-point end-point)
      (setq *narrowed-spaces* (~make-spaces n-spaces))

      ;; Remove all extra spaces
      (save-excursion
        (beginning-of-buffer)
        (loop until (eobp)
              do
              (unless (~current-line-empty?)
                (delete-forward-char n-spaces))
              (forward-line 1)))

      (let* ((language-mode-symbol (intern (s-concat language "-mode")))
             (mode-symbol          (cond ((~is-function-defined? language-mode-symbol)
                                          language-mode-symbol)
                                         (t
                                          (ht-get *language->mode-symbol*
                                                  language-mode-symbol
                                                  'markdown-mode)))))
        (funcall mode-symbol)))))

(defun ~narrow-to-code-region-asciidoc ()
  "Narrows to code region in Markdown mode.  Reindents."
  (interactive)
  (save-excursion
    (let* ((begin-code-region (rx bol "----" eol))
           (end-code-region (rx bol "----" eol))
           (language (save-excursion
                       (re-search-backward begin-code-region)
                       (previous-line)
                       (let* ((line (trim-spaces (thing-at-point 'line))))
                         (if (equalp "[source]" line)
                             "text"
                           (-> (split-string line "\\[source,")
                               second
                               (split-string ",")
                               first
                               (split-string "\\]")
                               first)))))
           (begin-point (save-excursion
                          (re-search-backward begin-code-region)
                          (next-line)
                          (beginning-of-line)
                          (point)))
           (n-spaces (save-excursion
                       (re-search-forward begin-code-region)
                       (let* ((line (thing-at-point 'line))
                              (spaces (progn
                                        (string-match (rx bol (group (0+ " ")))
                                                      line)
                                        (match-string 1 line))))
                         (length spaces))))
           (end-point (save-excursion
                        (re-search-forward end-code-region)
                        (previous-line)
                        (end-of-line)
                        (point))))
      (ht-set! *saved-major-mode-for-narrow* (buffer-file-name) major-mode)
      (narrow-to-region begin-point end-point)
      (setq *narrowed-spaces* (~make-spaces n-spaces))

      ;; Remove all extra spaces
      (save-excursion
        (beginning-of-buffer)
        (loop until (eobp)
              do
              (unless (~current-line-empty?)
                (delete-forward-char n-spaces))
              (forward-line 1)))

      (let* ((language-mode-symbol (intern (s-concat language "-mode")))
             (mode-symbol          (cond ((~is-function-defined? language-mode-symbol)
                                          language-mode-symbol)
                                         (t
                                          (ht-get *language->mode-symbol*
                                                  language-mode-symbol
                                                  'markdown-mode)))))
        (funcall mode-symbol)))))

(defalias '~narrow-to-code-region '~narrow-to-code-region-asciidoc)

(defun ~make-spaces (times)
  "Returns a string of `times' spaces."
  (s-join "" (loop for i upto (1- times) collect " ")))

(defun ~is-narrowed? ()
  "Checks if the buffer is still narrowed by `~narrow-to-code-region'."
  (not (null *narrowed-spaces*)))

(defun ~current-line-empty? ()
  "Determine if current line contains only whitespaces."
  (string-match-p (rx bol
                      (0+ whitespace)
                      eol)
                  (first (s-split "\n" (thing-at-point 'line)))))

(defun ~toggle-narrow-to-code-region ()
  "Toggle narrowing to region in Markdown mode by calling
`~narrow-to-code-region' or `~widen-narrowed-region'
respectively."
  (interactive)
  (cond ((null *narrowed-spaces*)
         (~narrow-to-code-region))
        (t
         (~widen-narrowed-region))))

(defun lp:save-snippet ()
  "Extract code snippets to a file.  To be called upon Markdown
code block."
  (interactive)
  (save-excursion
    (let* ((file-path (lp:extract-file-path))
           (begin-point (save-excursion
                          (cond ((~is-narrowed?)
                                 (beginning-of-buffer)
                                 (point))
                                (t
                                 (re-search-backward "```")
                                 (next-line)
                                 (beginning-of-line)
                                 (point)))))
           (end-point   (save-excursion
                          (cond ((~is-narrowed?)
                                 (end-of-buffer)
                                 (point))
                                (t
                                 (re-search-forward "```")
                                 (previous-line)
                                 (end-of-line)
                                 (point)))))
           (content     (buffer-substring begin-point end-point)))
      (~write-to-file file-path content)
      (message "Saved to %s" file-path))))

(defun lp:extract-file-path ()
  "Extracts file path from the first comment line."
  (save-excursion
    (let* ((raw-line (cond ((~is-narrowed?)
                            (beginning-of-buffer)
                            (thing-at-point 'line))
                           (t
                            (re-search-backward "```")
                            (next-line)
                            (thing-at-point 'line))))
           (line     (lp:remove-from-last-space raw-line)))
      (with-temp-buffer
        (insert line)
        (beginning-of-buffer)
        (cond ((or (ignore-errors (re-search-forward "%%path "))
                   (ignore-errors (re-search-forward "%%file ")))
               (let ((begin-point (point))
                     (end-point   (progn
                                    (end-of-line)
                                    (point))))
                 (buffer-substring begin-point end-point)))

              ((or (ignore-errors (re-search-forward "; "))
                   (ignore-errors (re-search-forward "# "))
                   (ignore-errors (re-search-forward "// "))
                   (ignore-errors (re-search-forward comment-start)))
               (let ((begin-point (point))
                     (end-point   (progn
                                    (end-of-line)
                                    (point))))
                 (buffer-substring begin-point end-point)))
              (t
               nil))))))

(defun lp:remove-from-last-space (str)
  ""
  str)

(defun lp:visit-snippet ()
  "Visit the snippet defined in current code block."
  (interactive)
  (let ((file-path (lp:extract-file-path)))
    (~smart-open-file file-path)))

;; FIXME: Support different kinds of block delimeters
(defun ~current-snippet->file (path)
  "Generates the corresponding file from current snippet."
  (interactive)
  (save-excursion
    (search-backward "[source")
    (next-line)
    (next-line)
    (beginning-of-line)
    (let ((start (point)))
      (search-forward "----")
      (previous-line)
      (end-of-line)
      (~write-to-file path (buffer-substring start (point))))
    (message "Written to %s" path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ulquikit integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ulqui:generate-html-current-dir ()
  "Generates HTML from and to current directory."
  (interactive)
  (message "Generating HTML...")
  (~exec-pop-up "ulqui generate-html --from . --to generated-html/"))

(defun ulqui:generate-src-current-dir ()
  "Generates source code from and to current directory."
  (interactive)
  (message "Generating source...")
  (~exec-pop-up "ulqui generate-src --from . --to generated-src/"))

(defun ~cl/next-snippet ()
  "Jumps to the next `eval'-able AsciiDoc snippet."
  (interactive)
  (cond ((re-search-forward "\\.\\(code\\|file\\).*\n\\[source,lisp" (point-max) t)
         (search-forward "----")
         (next-line)
         (beginning-of-line)
         (point))
        (t
         -1)))

(defun ~cl/compile-snippet ()
  "`eval's the current snippet with Common Lisp's Slime.  Note
that this function would not work reliably if the current point
is not inside a snippet."
  (interactive)
  (save-excursion
    (cond ((member major-mode '(lisp-mode common-lisp-mode))
           (beginning-of-buffer)
           (let ((start (point)))
             (end-of-buffer)
             (slime-compile-region start (point))))
          (t
           (re-search-backward "^----$")
           (next-line)
           (beginning-of-line)
           (let ((start (point)))
             (re-search-forward "^----$")
             (previous-line)
             (end-of-line)
             (slime-compile-region start (point)))))))

(defun ulqui:tmux-make ()
  "Runs `make' in current Tmux session."
  (interactive)
  (srun "make"))

(with-eval-after-load "adoc-mode"
  (bind-key "<f5>" '~cl/next-snippet adoc-mode-map)
  (bind-key "<f6>" '~cl/compile-snippet adoc-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "s-; n n" '~toggle-narrow-to-code-region)
(bind-key "s-; n r" '~narrow-to-code-region)
(bind-key "s-; n w" '~widen-narrowed-region)

(bind-key "s-; s s" 'lp:save-snippet)
(bind-key "s-; s v" 'lp:visit-snippet)

(bind-key "<S-f5>" 'ulqui:generate-src-current-dir)
(bind-key "<S-f6>" 'ulqui:generate-html-current-dir)
(bind-key "C-S-b"  'ulqui:tmux-make)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading literate programming facility")

(provide 'rmacs:config-literate-prog)
