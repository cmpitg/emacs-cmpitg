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

(defun* ~string-start-with? (string substring &key (ignore-case nil))
  "Determine if a string starts with a substring.

E.g.

\(~string-start-with? \"config-default\" \"config-\"\)  ;; => t
\(~string-start-with? \"config-default\" \"Config-\"\)  ;; => nil"
  (s-starts-with? substring string ignore-case))

(defun* ~string-end-with? (string substring &key (ignore-case nil))
  "Determine if a string ends with a substring.

E.g.

\(~string-end-with? \"config-default\" \"default\"\)  ;; => t
\(~string-end-with? \"config-default\" \"Default\"\)  ;; => nil"
  (s-ends-with? substring string ignore-case))

(defun ~string-empty? (str)
  "Determine if a string is empty.  `nil' is treated as empty
string."
  (or (null str)
      (and (stringp str)
           (= 0 (length str)))))

(defun ~string-but-last (str)
  "Return a string with its last character removed."
  (if (~string-empty? str) ""
      (substring str 0 -1)))

(defun ~string-contains? (str substring)
  "Check if a string contains a substring."
  (not (null (string-match substring str))))

(defun ~symbol->string (symbol)
  "Convert a symbol to a string."
  (symbol-name symbol))

(defun ~string->symbol (string)
  "Convert a string into an uninterned symbol."
  (make-symbol string))

(defun ~join-strings (separator a-seq)
  "Join strings.  Works with any type of sequence and any data type as its element.

E.g.

\($join-strings \"|\" '\(\"a\" \"b\" \"c\"\)\) ; => \"a|b|c\"
\($join-strings \"|\" [1 \"b\" c]\) ; => \"1|b|c\""
  (-reduce (lambda (result element)
             (format "%s%s%s" result separator element))
           (-map (lambda (x) x) a-seq)))

(defun ~trim-spaces (text)
  "Trim spaces at the beginning and the end of a portion of
text."
  (while (and (not (~string-empty? text))
              (string= " " (~first-char-as-string text)))
    (setf text (substring text 1)))

  (while (and (not (~string-empty? text))
              (string= " " (~last-char-as-string text)))
    (setf text (substring text 0 (- (length text) 1))))
  text)

(defun ->string (exp)
  "Convert an expression to string with `format'."
  (format "%s" exp))

(defalias 'string-empty? '~string-empty?)
(defalias 'string-start-with? '~string-start-with?)
(defalias 'string-end-with? '~string-end-with?)
(defalias 'string-empty? '~string-empty?)
(defalias 'string-but-last '~string-but-last)
(defalias 'string-contains? '~string-contains?)
(defalias 'symbol->string '~symbol->string)
(defalias 'string->symbol '~string->symbol)
(defalias 'join-strings '~join-strings)
(defalias 'trim-spaces '~trim-spaces)
