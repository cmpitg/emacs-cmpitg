;; -*- no-byte-compile: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Create a temporary file and save
;; - Create a temporary file, save, then kill the buffer to see if the file is
;;   deleted
;; - Create a temporary file, save, close, open again, the kill the buffer to
;;   see if the file is deleted
;; - Create a temporary file, change the value of local/delete-on-close to nil
;;   save, reload, then kill the buffer to see if the file is not deleted

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Call <S-f3> in a directory and see if Projectile chooses the correct
;;   project directory and ignore the correct patterns
;; - Call <C-f3> in a directory and see if current dir or current project dir
;;   is taken into account and if it ignores the correct patterns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Firefox
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - Call `~firefox`
;; - Call `~firefox` with a selection which is a URL
;; - Call `~firefox` with a cursor under a URL

;; https://yle.fi
;; https://areena.yle.fi

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asciidoc mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create/open an Asciidoc file and try:
;; - Previewing
;; - Making change then previewing
;; - Rendering to a specific location

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *exec* functions & calling external processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Execute a command and return the output as string.
;; Quick pattern: <not-possible>
(~exec "ls")

;; Execute a command synchronously, taking current region as stdin and return
;; the output as string.
;; Quick pattern: <not-possible>
(~exec "cat" :stdin :region)

;; Execute a command synchronously, taking current region as stdin and return
;; the output as string.
;; Quick pattern: <not-possible>
(~exec "cat" :stdin "Some\ncrap")

;; Execute a command and pop up a temporary buffer showing result.
;; Quick pattern: ls
(~exec-pop-up "ls -lahF / /mnt")

;; Execute a command, taking input from the current region and pop up a
;; temporary buffer showing result.
;; Test: select any text in any window, then `eval-expression` the following.
;; Quick pattern: >cat
(~exec> "cat")
(~exec> "no-command")

;; Execute a command, taking input from the current region replace it with
;; result once the command is done.
;; Test: select any text in any window, then `eval-expression` the following.
;; Quick pattern: |sort
(~exec| "cat")
(~exec| "sort")
(~exec| "no-command")

;; b
;; a
;; d
;; c

;; Execute a command and replace current region with the result once the
;; command is done.
;; Test: Place the cursor 
;; Quick pattern: <ls
(~exec< "date")
(~exec< "date -R")

;; Run an external processes
(~run-process (format "web-browser-gui %s"
                      (shell-quote-argument "https://yle.fi")))

(~run-process (format "web-browser-gui %s"
                      (shell-quote-argument "https://yle.fi"))
              :async nil)

;; Open with an external program
(~open-with "https://svenska.yle.fi/" "web-browser-gui %s")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Click & execute
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; b
;; a
;; e
;; c
;; d

;; <ls
;; <date
;; <date -R
;; >cat
;; >sort
;; >rot13
;; |sort
;; |rot13
