;;  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Ha-Duong Nguyen (@cmpitg)
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

(defun aws/kms-encrypt (key text)
  (interactive "MKey ID/alias: \nMText: ")
  (~exec (format "aws kms encrypt --key-id '%s' --plaintext '%s' --query CiphertextBlob"
                                 key
                                 text)))

(defun aws/kms-decrypt (text)
  (interactive "MText: ")
  (~exec (format "bash -c 'aws kms decrypt --ciphertext-blob fileb://<(echo \'%s\' | base64 -d) --output text --query Plaintext | base64 -d'"
                      text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading AWS helpers")

(provide 'rmacs:functions-aws)
