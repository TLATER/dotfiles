;;; erc.el --- ERC configuration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tristan Daniël Maat

;; Author: Tristan Daniël Maat <tm@tlater.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package tls
             :config
             (add-to-list 'tls-program "openssl s_client -connect %h:%p -no_ssl2 -ign_eof -cert $HOME/.config/certificates/%h/tlater.pem"))

(use-package erc
             :after (tls)
             :commands erc-tls
             :init
             (setq erc-log-channels-directory "~/.emacs.d/erc/logs")
             (setq erc-keywords '("NB ALL:"))
             (setq erc-modules '(autojoin button completion fill
                                 irccontrols list log match menu
                                 move-to-prompt netsplit networks
                                 noncommands notifications
                                 readonly ring scrolltobottom
                                 stamp spelling track truncate))
             :config
	     (make-directory "~/.emacs.d/erc/logs" t)
             (defun erc-codethink ()
               (interactive)
               (erc-tls :server "irc0.codethink.co.uk"
                        :port 6502
                        :nick "tristanmaat/codethink"
                        :full-name user-full-name)))


(provide 'erc)
;;; erc.el ends here
