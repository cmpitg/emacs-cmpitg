;;
;; Copyright (C) 2014-2017 Ha-Duong Nguyen (@cmpitg)
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
;; HTTP-based IPC with emnode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *emnode-routes*
      '(("^.*//eval/?"         . ~ipc-eval)
        ("^.*//open/\\(.*\\)"  . ~ipc-open-file)
        ("^.*//exec/\\(.*\\)"  . ~ipc-exec-file)))

(defun ~ipc-eval (httpcon)
  (let* ((expr (format "%s" (emnode:http-data httpcon))))
    (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
    (unless (~string-empty? (s-trim expr))
      (emnode:http-end httpcon (or (ignore-errors (format "%s" (~add-bracket-and-eval expr)))
                                   "")))))

(defun ~ipc-open-file (httpcon)
  (let ((path (emnode:http-get-arg httpcon 1)))
    (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
    (emnode:http-end httpcon (format "Opening: %s\n" path))
    (toolbox:open-file path :new-frame? (emnode-http-param httpcon "new-frame"))))

(defun ~ipc-exec-file (httpcon)
  (let ((path (emnode:http-get-arg httpcon 1)))
    (emnode:http-start httpcon 200 '("Content-Type" . "text/plain"))
    (emnode:http-end httpcon (format "Executing: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (eval-buffer))))

;; $ curl 0:9999/eval/ -d 'message-box "Hello World"'
;; $ curl 0:9999/eval -d '(message-box "Hello") (message-box "World")'
;; $ curl 0:9999/open//m/src
;; $ curl 0:9999/exec//tmp/tmp.el

(eval-and-compile
  (defun cmpitg/emnode-load-path ()
    (~get-config "local-packages/emnode")))

(use-package emnode
  :load-path (lambda () (list (cmpitg/emnode-load-path)))
  :ensure elnode
  :config
  (progn
    (setq emnode:*log-level* emnode:+log-none+)
    (emnode:stop *emacs-server-port*)
    (ignore-errors
      (emnode:start-server *emnode-routes* :port *emacs-server-port*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish loading IPC capability")
(provide 'ee:config-ipc)
