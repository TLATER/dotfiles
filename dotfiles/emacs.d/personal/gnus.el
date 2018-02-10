(require 'gnus)

(setq mail-source-directory "~/Documents/Mail/")
(setq mail-source-idle-time-delay 600)

(setq gnus-select-method
      '(nnimap "tlater.net"
               (nnimap-address "imap.tlater.net")
               (nnimap-server-port "imaps")
               (nnimap-stream ssl)))

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "kiara.at"
                      (nnimap-address "imap.kiara.at")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "codethink"
                      (nnimap-address "mail.codethink.co.uk")
                      (nnimap-server-port "imaps")
                      (nnimap-stream ssl)))

(setq smtpmail-smtp-server "mail.codethink.co.uk"
      smtpmail-smtp-service 465
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
