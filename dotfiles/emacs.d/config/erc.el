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

(use-package erc
  :commands (erc-tls erc-buffer-list)
  :init
  (setq erc-log-channels-directory (expand-file-name "erc/logs" data-dir))
  (setq erc-keywords '("NB ALL:"))
  (setq erc-modules '(autojoin button completion fill
                               irccontrols list log match menu
                               move-to-prompt netsplit networks
                               noncommands notifications
                               readonly ring scrolltobottom
                               stamp spelling track truncate))
  :config
  (require 'erc-spelling)
  (erc-spelling-mode 1)
  (make-directory erc-log-channels-directory t)
  (defun erc-codethink ()
    (interactive)
    (erc-tls :server "bouncer.codethink.co.uk"
             :port 6502
             :nick "tristanmaat/codethink"
             :full-name user-full-name))
  (defun erc-exirc ()
    (interactive)
    (erc-tls :server "bouncer.codethink.co.uk"
             :port 6502
             :nick "tristanmaat/exirc"
             :full-name user-full-name)))


(provide 'erc)
;;; erc.el ends here
