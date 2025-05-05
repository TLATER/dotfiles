;;; init.el --- Emacs configuration entry point      -*- lexical-binding: t; -*-

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

;; Sets up some basic variables (directory paths etc.), as well as packages and features
;; that need to be set at the very start of initialization.  After that, load the rest
;; of the configuration files.

;;; Code:

;; ----------------------------------------------------------------------------------
;;; Basic path setup
;; ----------------------------------------------------------------------------------

(defvar back-dir (expand-file-name
                  "emacs/backups"
                  (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defvar share-dir (expand-file-name
                   "emacs/share"
                   (or (getenv "XDG_CONFIG_HOME") "~/.config")))
(defvar cache-dir (expand-file-name
                   "emacs"
                   (or (getenv "XDG_CACHE_HOME") "~/.cache")))

;; Load `custom.el'' for any imperative settings; This sets safe
;; themes and variables, which is important to get out of the way
;; early.
(setq custom-file (expand-file-name "custom.el" data-dir))
(if (file-exists-p custom-file)
    (load custom-file)
  (custom-set-variables))

;; ----------------------------------------------------------------------------------
;;; Package management
;; ----------------------------------------------------------------------------------

(require 'package)
(setq package-user-dir (expand-file-name "elpa" data-dir))
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(require 'leaf)

;; Make sure emacs doesn't try to randomly fetch packages itself; nix is in charge of
;; that
(setq package-enable-at-startup nil)

;; ----------------------------------------------------------------------------------
;;; Set up improved garbage collection for better startup and runtime performance.
;; ----------------------------------------------------------------------------------

(leaf gcmh
  :ensure t
  :global-minor-mode t)

;; ----------------------------------------------------------------------------------
;;; Load other configuration files
;; ----------------------------------------------------------------------------------

;; Load everything in the config directory
(let ((config-dir (expand-file-name "config" (file-name-directory load-file-name))))
  (when (file-exists-p config-dir)
    (mapc (lambda (file)
            (load (file-name-sans-extension file) nil nil nil t))
          (directory-files config-dir t "^[^#\.].*\\.el$"))))

;;; init.el ends here
