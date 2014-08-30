;;
;; Copyright (C) 2013-2014 Duong Nguyen ([@cmpitg](https://github.com/cmpitg/))
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

(defun ~add-emacs-lisp-header ()
  "Add Emacs Lisp header."
  (interactive)
  (insert ";;; mylib.el --- 
;; 
;; Filename: mylib.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Copyright (C) 2014  Duong Nguyen
;; Created: $ date -R
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mylib.el ends here"))

(defun* ~add-gplv3-header (&key author
                               project-name
                               year)
  "Add GLPv3 header with

*  `author' as the copyright holder,
* `project-name' as the name of the project, and
* `year'

For each argument, if it's not provided, a prompt would appear
for you to input.

If `project-name' is an empty string, it means your file a
single-file project.  Thus the first line \"This file is a port
of ...\" would not be added to the header.

`$add-gplv3-header' knows to handle comments properly base on
variables `comment-start' and `comment-end'.

TODO: Handling comment when `comment-end' is not empty.
"
  (interactive)
  (let* ((author (if author
                   author
                   (read-string "Author: ")))
         (project-name (if project-name
                         project-name
                         (read-string "Project name: ")))
         (year (if year
                 year
                 (read-string "Year: ")))
         
         (project-name-exists? (if (or (null project-name)
                                       (= 0 (length project-name)))
                                 nil
                                 t))
         (this-file-or-project-name (if project-name-exists?
                                      project-name
                                      "This file"))
         (this-file-or-project-name-lowcase (s-downcase this-file-or-project-name))

         ;; Comment
         (comment (if (s-equals? comment-start ";")
                    ";;"
                    comment-start))

         (first-line (s-lex-format
                      "${comment} This file is part of ${project-name} project."))
         (gplv3-header-text (s-lex-format "${comment}
${comment} Copyright (C) ${year}  ${author}
${comment} 
${comment} ${this-file-or-project-name} is free software: you can redistribute it and/or modify
${comment} it under the terms of the GNU General Public License as published by
${comment} the Free Software Foundation, either version 3 of the License, or
${comment} \(at your option\) any later version.
${comment} 
${comment} ${this-file-or-project-name} is distributed in the hope that it will be useful,
${comment} but WITHOUT ANY WARRANTY; without even the implied warranty of
${comment} MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
${comment} GNU General Public License for more details.
${comment} 
${comment} You should have received a copy of the GNU General Public License
${comment} along with ${this-file-or-project-name-lowcase}.  If not, see <http://www.gnu.org/licenses/>.
${comment}
"))

         (header-text (if project-name-exists?
                        (s-lex-format "${comment}
${first-line}
${gplv3-header-text}")
                        gplv3-header-text)))
    (message-box "%s %s" project-name-exists? (length project-name))
    (insert header-text)
    (message "Done, you might want to reformat your header.")))

(defalias 'add-emacs-lisp-header '~add-emacs-lisp-header)
(defalias 'add-gplv3-header '~add-gplv3-header)
