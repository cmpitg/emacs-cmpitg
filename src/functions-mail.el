;;
;; Copyright (C) 2017 Ha-Duong Nguyen (@cmpitg)
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

(defun* ~get-mail-header (header msg-or-path &key (from-path nil))
  "Retrieves a specific header field from a full mail."
  (let ((cmd (concat (format " sed -n '/^%s:/I{:loop t;h;n;/^ /{H;x;s/\\n//;t loop};x;p}' " header)
                     (if from-path msg-or-path "")
                     " | "
                     (format " sed -n 's/^%s: \\(.*\\)$/\\1/Ip' " header))))
    (--> (if from-path
             (~exec cmd)
           (~exec-with-input cmd msg-or-path))
         (replace-regexp-in-string "[ \t\n]*$" "" it))))

(defun* ~get-mail-user-agent (msg-or-path &key (from-path nil))
  "Retrieves the User-Agent of the mail sender."
  (let ((x-mailer   (~get-mail-header "x-mailer" msg-or-path :from-path from-path))
        (user-agent (~get-mail-header "user-agent" msg-or-path :from-path from-path)))
    (cond
     ((string= x-mailer user-agent) user-agent)
     ((string= x-mailer "")         user-agent)
     ((string= user-agent "")       x-mailer)
     (t (format "%s (x-mailer)\n%s (user-agent)" x-mailer user-agent)))))

(defun* ~send-mail-with-thunderbird (&key (to "") (subject "") (body ""))
  "Sends email with Thunderbird."
  (~exec (format "thunderbird-bin -compose \"to='%s',subject='%s'\",body=\"'%s'\""
                 to
                 subject
                 body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading mail processing functions")
(provide 'ee:functions-mail)
