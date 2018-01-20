;;
;; Copyright (C) 2017-2018 Ha-Duong Nguyen (@cmpitg)
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
;; Email with mu4e - Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; * cd local-packages/mu
;;   autoreconf -i
;;   ./configure
;;   make
;; * /m/mail
;; * ~/.authinfo.gpg
;;
;; Check if Mu is working
;;   $ mu index --maildir=/m/mail/boxes
;;   $ mu find hello
;;

(eval-and-compile
  (defun cmpitg/mu4e-load-path ()
    (~get-config "local-packages/mu/mu4e")))

;; (set-frame-font "Inconsolata" nil t)

(defun* cmpitg:add-mu4e-account (&key context-name
                                      full-name
                                      mail-address
                                      match-recipients
                                      default-headers
                                      smtp-server
                                      sent-folder
                                      signature-file
                                      maildir-shortcuts)
  "Add a mu4e mail account.

E.g.

\(cmpitg:add-mu4e-account :context-name \"cmpitg@gmail\"
                         :full-name \"Ha-Duong Nguyen\"
                         :mail-address \"cmpitg@gmail.com\"
                         :match-recipients '\(\"cmpitg.gmail.com\"\)
                         :default-headers \"BCC: cmpitg@gmail.com\"
                         :smtp-server \"smtp.gmail.com:587\"
                         :sent-folder \"/cmpitg-at-gmail/sent\"
                         :signature-file \"/m/mail/signature_cmpitg-at-gmail.txt\"
                         :maildir-shortcuts '\(\"cmpitg-at-gmail/Inbox\" . ?i\)\)"
  ;; Because Emacs uses dynamic binding by default
  (lexical-let ((context-name context-name)
                (full-name full-name)
                (mail-address mail-address)
                (match-recipients match-recipients)
                (default-headers default-headers)
                (smtp-server smtp-server)
                (sent-folder sent-folder)
                (signature-file signature-file)
                (maildir-shortcuts maildir-shortcuts))
    (add-to-list 'mu4e-user-mail-address-list mail-address t)
    (destructuring-bind (smtp-server smtp-port) (s-split ":" smtp-server)
      (add-to-list 'mu4e-contexts
                   (make-mu4e-context
                    :name context-name
                    :enter-func (lambda () (mu4e-message (format "Context: %s" context-name)))
                    :match-func (lambda (msg) nil)
                    :vars `((mu4e-reply-to-address . ,mail-address)
                            (user-mail-address . ,mail-address)
                            (user-full-name . ,full-name)
                            (mail-default-headers . ,default-headers)
                            (mu4e-compose-signature . ,(~read-file signature-file))
                            (smtpmail-mail-address . ,mail-address)
                            (smtpmail-default-smtp-server . ,smtp-server)
                            (smtpmail-smtp-user . ,mail-address)
                            (smtpmail-smtp-server . ,smtp-server)
                            (smtpmail-smtp-service . ,smtp-port)
                            (smtpmail-starttls-credentials . ((,smtp-server ,smtp-port ,mail-address nil)))
                            (mu4e-sent-folder . ,sent-folder)
                            (mu4e-maildir-shortcuts . ,maildir-shortcuts)))
                   t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading mu4e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mu4e
  :load-path (lambda () (list (cmpitg/mu4e-load-path)))
  :config
  (progn
    ;; TODO - Context menu

    (add-hook 'mu4e-compose-mode-hook '~turn-on-soft-wrapping)

    ;; Use Helm instead of ido
    (use-package helm-mu
      :ensure t
      :config (progn
                (define-key mu4e-main-mode-map "s" 'helm-mu)
                (define-key mu4e-headers-mode-map "s" 'helm-mu)
                (define-key mu4e-view-mode-map "s" 'helm-mu)))
    (setq mu4e-completing-read-function completing-read-function)

    ;; Add user-agent to mail view
    (add-to-list 'mu4e-header-info-custom
                 '(:user-agent . (:name "User-Agent"
                                        :shortname "UserAgent"
                                        :help "User Agent of sender"
                                        :function (lambda (mu4e-msg)
                                                    (let ((path (or (mu4e-message-field mu4e-msg :path)
                                                                    "")))
                                                      (if (not (file-readable-p path))
                                                          "Mail file is not accessible"
                                                        (~get-mail-user-agent path :from-path t)))))))

    ;; Add long date format
    (add-to-list 'mu4e-header-info-custom
                 '(:long-date . (:name "Long-Date"
                                       :shortname "LongDate"
                                       :help "Date & time of sending, long, readable format"
                                       :function (lambda (mu4e-msg)
                                                   (let ((date&time (mu4e-msg-field mu4e-msg :date)))
                                                     ;; (format-time-string "%a, %d %b %Y %H:%M:%S" date&time)
                                                     (format-time-string "%a %d.%m.%Y %H:%M:%S" date&time))))))

    ;; (setq mu4e-get-mail-command "mbsync -c /m/mail/mbsyncrc -a")
    ;; (setq mu4e-get-mail-command "mbsync -c /m/mail/mbsyncrc cmpitg-gmail_useful hd-at-mamk_all")
    (setq mu4e-get-mail-command "sync-mails")

    (setq mu4e-confirm-quit nil)
    (setq mu4e-use-fancy-chars nil)

    ;; Try displaying images
    (setq mu4e-view-show-images t)
    (setq mu4e-view-image-max-width 800)

    (setq mu4e-maildir "/m/mail/boxes/")
    (setq mu4e-attachment-dir "~/Downloads")

    (setq mu4e-headers-results-limit 3000)
    (setq mu4e-view-fields '(:from
                             :to :cc :subject :user-agent :flags :date
                             :maildir :mailing-list :tags :attachments
                             :signature :decryption))

    (setq mu4e-headers-fields `((:long-date . 23)
                                (:flags . 6)
                                (:from . 30)
                                (:to . 25)
                                (:subject)))

    ;; If changed, restart mu4e
    (setq mu4e-update-interval 180)
    ;; (setq mu4e-update-interval nil)

    (setq mu4e-sent-folder   "/sent"
          mu4e-drafts-folder "/draft"
          mu4e-trash-folder  "/trash")

    (setq message-send-mail-function     'smtpmail-send-it)
    (setq send-mail-function             'smtpmail-send-it)
    (setq smtpmail-default-smtp-server   "smtp.gmail.com")
    (setq smtpmail-smtp-server           "smtp.gmail.com")
    (setq smtpmail-smtp-service          587)
    (setq smtpmail-starttls-credentials  '(("smtp.gmail.com" 587 nil nil)))
    (setq smtpmail-debug-info            t)
    (setq smtpmail-debug-verb            t)
    (setq starttls-gnutls-program        "/usr/bin/gnutls-cli")
    (setq starttls-extra-arguments       nil)
    (setq starttls-use-gnutls            t)
    (setq smtpmail-auth-credentials      "~/.authinfo.gpg")
    (setq auth-sources                   '("~/.authinfo.gpg"))


    (setq mu4e-headers-skip-duplicates t)

    ;; Save messages to Sent Messages, Gmail/IMAP doesn't take care of this
    ;; as the docs say
    ;; (setq mu4e-sent-messages-behavior 'delete)
    (setq mu4e-sent-messages-behavior 'sent)

    ;; If you need offline mode, set these -- and create the queue dir
    ;; with 'mu mkdir', i.e. mu mkdir <path-to-queue>
    (setq smtpmail-queue-mail  nil)
    (setq smtpmail-queue-dir   "/m/mail/queue/cur")

    (setq mu4e-html2text-command "lynx -dump -stdin")
    ;; (setq mu4e-html2text-command "w3m -T text/html")
    (setq mu4e-view-show-images t)

    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)
    (setq browse-url-browser-function (lambda (url &rest args)
                                        (~firefox url :new-window? t)))

    ;; Don't keep message buffers around
    (setq message-kill-buffer-on-exit t)

    (setq mu4e-user-mail-address-list (list))
    (setq mu4e-contexts (list))
    (cmpitg:add-mu4e-account :context-name "cmpitg@gmail"
                             :full-name "Ha-Duong Nguyen"
                             :mail-address "cmpitg@gmail.com"
                             :match-recipients '("cmpitg@gmail.com" "nha.duong@gmail.com")
                             :default-headers "BCC: cmpitg@gmail.com"
                             :smtp-server "smtp.gmail.com:587"
                             :signature-file "/m/mail/signature_cmpitg-at-gmail.txt"
                             :sent-folder "/cmpitg-at-gmail/sent"
                             :maildir-shortcuts '(("/cmpitg-at-gmail/Inbox"          . ?i)
                                                  ("/cmpitg-at-gmail/top-todo"       . ?t)
                                                  ("/cmpitg-at-gmail/top-delegated"  . ?d)
                                                  ("/cmpitg-at-gmail/top-awaiting"   . ?a)
                                                  ("/cmpitg-at-gmail/draft"          . ?r)
                                                  ("/cmpitg-at-gmail/sent"           . ?s)))
    (cmpitg:add-mu4e-account :context-name "hd@bayo"
                             :full-name "Ha-Duong Nguyen"
                             :mail-address "hd@bayo.vn"
                             :match-recipients '("hd@bayo.vn")
                             :default-headers "BCC: cmpitg@gmail.com, hd@bayo.vn"
                             :smtp-server "mail.securemail.vn:587"
                             :signature-file "/m/mail/signature_hd-at-bayo.txt"
                             :sent-folder "/cmpitg-at-gmail/sent"
                             :maildir-shortcuts '(("/cmpitg-at-gmail/Inbox"          . ?i)
                                                  ("/cmpitg-at-gmail/top-todo"       . ?t)
                                                  ("/cmpitg-at-gmail/top-delegated"  . ?d)
                                                  ("/cmpitg-at-gmail/top-awaiting"   . ?a)
                                                  ("/cmpitg-at-gmail/draft"          . ?r)
                                                  ("/cmpitg-at-gmail/sent"           . ?s)))
    (cmpitg:add-mu4e-account :context-name "m-hd@mamk"
                             :full-name "Ha-Duong Nguyen"
                             :mail-address "Duong.Nguyen2@metropolia.fi"
                             :match-recipients '("Duong.Nguyen2@metropolia.fi")
                             :default-headers "BCC: cmpitg@gmail.com, Duong.Nguyen2@metropolia.fi"
                             :smtp-server "smtp.metropolia.fi:587"
                             :signature-file "/m/mail/signature_hd-at-mamk.txt"
                             :sent-folder "/hd-at-mamk/sent"
                             :maildir-shortcuts '(("/hd-at-mamk/Inbox"          . ?i)
                                                  ("/hd-at-mamk/Drafts"         . ?r)))
    (cmpitg:add-mu4e-account :context-name "b-hd@buddify"
                             :full-name "Ha-Duong Nguyen"
                             :mail-address "haduong.nguyen@buddify.io"
                             :match-recipients '("haduong.nguyen@buddify.io" "duong.nguyen@buddify.io")
                             :default-headers "BCC: haduong.nguyen@buddify.io"
                             :smtp-server "smtp.zoho.com:587"
                             :signature-file "/m/mail/signature_hd-at-buddify.txt"
                             :sent-folder "/hd-at-buddify/sent"
                             :maildir-shortcuts '(("/hd-at-buddify/Inbox"          . ?i)
                                                  ("/hd-at-buddify/Drafts"         . ?r)))))

(bind-key "<M-f12>" #'mu4e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Finish configuring mail")

(provide 'rmacs:config-mail)
