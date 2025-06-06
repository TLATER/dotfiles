;;; integration.el --- OS integration settings      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tristan Daniël Maat

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

(eval-and-compile
  (require 'leaf))

;; ----------------------------------------------------------------------------------
;;; User settings
;; ----------------------------------------------------------------------------------

(setq user-full-name "Tristan Daniël Maat")
(setq user-mail-address "tm@tlater.net")

;; ----------------------------------------------------------------------------------
;;; Authentication
;; ----------------------------------------------------------------------------------

(leaf auth-source
  :custom
  (auth-sources . '("secrets:Personal")))

;; ----------------------------------------------------------------------------------
;;; External applications
;; ----------------------------------------------------------------------------------

(leaf browse-url
  :commands browse-url
  :custom
  (browse-url-browser-function . 'browse-url-default-browser))

(leaf direnv
  :ensure t
  :global-minor-mode direnv-mode)

;; ----------------------------------------------------------------------------------
;;; XDG dirs
;; ----------------------------------------------------------------------------------

(leaf eshell
  :commands eshell
  :defvar data-dir
  :custom
  `(eshell-directory-name . ,(expand-file-name "eshell" data-dir)))

(leaf recentf
  :defun recentf-save-list
  :custom
  `(recentf-save-file . ,(expand-file-name "recentf" data-dir))
  :config
  (add-hook 'delete-frame-functions (lambda (_) (recentf-save-list))))

(leaf tramp
  :custom
  `(tramp-persistency-file-name . ,(expand-file-name "tramp" data-dir))
  (tramp-default-method . "scp"))

(leaf url
  :custom
  `(url-configuration-directory . ,(expand-file-name "url" data-dir)))

;;; integration.el ends here
