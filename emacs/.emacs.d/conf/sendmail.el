;;; sendmail.el---Sendmail configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 09 Jul 2016
;;
;;; Code:

(use-package sendmail
  :defines message-send-mail-function message-sendmail-extra-arguments
  :init
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-extra-arguments '("-C" "/home/tlater/.config/msmtp/msmtprc")))

;;; sendmail.el ends here
