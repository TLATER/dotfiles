;;; erc.el --- ERC configuration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniël Maat

;; Author: Tristan Daniël Maat <tm@tlater.net>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Highlights
(require 'erc)
(require 'erc-match)
(setq erc-keywords '("NB ALL:"))

;; Set tls program
(require 'tls)
(setq tls-program (cons "openssl s_client -connect %h:%p -no_ssl2 -ign_eof -cert $HOME/.config/certificates/%h/tlater.pem" tls-program))

(defun erc-codethink ()
  (interactive)
  (erc-tls :server "irc0.codethink.co.uk"
           :port 6502
           :nick "tristanmaat/codethink"
           :full-name user-full-name))

(defun erc-gnome ()
  (interactive)
  (erc-tls :server "irc0.codethink.co.uk"
           :port 6502
           :nick "tristanmaat/gnome"))

(defun erc-buildstream ()
  (interactive)
  (erc :server "irc.gnome.org"))

(defun erc-freenode ()
  (interactive)
  (erc-tls :server "irc0.codethink.co.uk"
           :port 6502
           :nick "tristanmaat/freenode"))

(setq erc-autojoin-channels-alist
      '(("irc0.codethink.co.uk" "#codethink" "#bloomberg")
        ("irc.gnome.org" "#buildstream")))

;; Make ERC column fill change with window size
;; (https://www.emacswiki.org/emacs/ErcFilling#toc2)
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

(provide 'erc)
;;; erc.el ends here
