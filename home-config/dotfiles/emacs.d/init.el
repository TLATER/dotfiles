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


(defvar config-dir (expand-file-name
                    "config"
                    (file-name-directory load-file-name)))
(defvar data-dir (expand-file-name
                  "emacs"
                  (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defvar back-dir (expand-file-name
                  "emacs/backups"
                  (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defvar share-dir (expand-file-name
                   "share"
                   (file-name-directory load-file-name)))

;; ----------------------------------------------------------------------------------
;;; Package management
;; ----------------------------------------------------------------------------------

(require 'package)
(setq package-user-dir (expand-file-name "elpa" data-dir))
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

;; Setup use-package
;; bind-key is a dependency...
(require 'bind-key)
(require 'use-package)

;; Make sure emacs doesn't try to randomly fetch packages itself; nix is in charge of
;; that
(setq package-enable-at-startup nil)
(setq use-package-always-ensure nil)
(setq use-package-ensure-function 'ignore)

;; Avoid all kinds of byte compilation warnings - see
;; https://github.com/jwiegley/use-package/issues/590
(eval-when-compile
  (setq use-package-expand-minimally byte-compile-current-file))

;; ----------------------------------------------------------------------------------
;;; Set up improved garbage collection for better startup and runtime performance.
;; ----------------------------------------------------------------------------------

(use-package gcmh
  :config
  (gcmh-mode 1))

;; ----------------------------------------------------------------------------------
;;; Load other configuration files
;; ----------------------------------------------------------------------------------

;; Make sure we load the *newer* config files, be they .elc or .el files, instead of
;; always trying to load compiled files and confusing the hell out of me when I try to
;; make config changes and they don't apply.
(setq load-prefer-newer t)

;; Load everything in the config directory
(when (file-exists-p config-dir)
  (mapc (lambda (file)
          (load file nil nil t))
        (directory-files config-dir 't "^[^#\.].*\\.el$")))

;; Finally load `custom.el'' for any declarative settings; there *should* be none, but
;; occasionally I need something a bit less rigid than my nix config for a little while.
(setq custom-file (expand-file-name "custom.el" data-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
