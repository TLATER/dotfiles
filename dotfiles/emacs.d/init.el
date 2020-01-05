;;; init.el --- Emacs configuration entry point      -*- lexical-binding: t; -*-

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

;; Set configuration locations
(defvar config-dir (expand-file-name
                    "config"
                    (file-name-directory load-file-name)))
(defvar theme-dir (expand-file-name
                   "themes"
                   (file-name-directory load-file-name)))
(defvar backup-dir (expand-file-name
                    "backups"
                    (file-name-directory load-file-name)))
(defvar share-dir (expand-file-name
                   "share"
                   (file-name-directory load-file-name)))
(setq custom-file (expand-file-name "custom.el" config-dir))

;; Disable the useless UI components
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Set some other annoyances that need to be rid of in all cases
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(when (display-graphic-p)
  (put 'suspend-frame 'disabled t))
(setq load-prefer-newer t)
(setq large-file-warning-threshold 100000000)

;; Don't litter backup files everywhere
(setq backup-by-copying t
      backup-directory-alist
      '((".*" . "~/.emacs.d/backups/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list" share-dir))
(setq create-lockfiles nil)

;; Set the theme
(add-to-list 'custom-theme-load-path theme-dir)
(load-theme 'cyan t)

;; Might not want to do the latter if we're not in a normal
;; environment, i.e., if we're *not* invoked as a daemon. Maybe set
;; some minimal components for daemon mode and stop here if we're
;; root?

;; Figure out if package installation is handled externally.
;;
;; We do this by checking if any installed use-package version is
;; marked as "external".
(eval-and-compile
  (require 'package)
  (setq using-external-packages (member "external"
                          (mapcar 'package-desc-status
                                  (cdr (assoc 'use-package package-alist))))))

;; Make sure use-package is installed if it's not installed externally
(unless using-external-packages
  (unless package--initialized (package-initialize))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(eval-and-compile
  ;; Setup use-package, regardless of how it is installed
  ;; bind-key is a dependency...
  (require 'bind-key)
  (require 'use-package)

  ;; If packages are installed externally, we want to turn "ensure" off
  (setq use-package-always-ensure (not using-external-packages))
  (when using-external-packages
    (setq use-package-ensure-function 'ignore)
    (setq package-enable-at-startup nil)))

;; Ensure that our exec path is set up correctly
(use-package exec-path-from-shell
  :functions (exec-path-from-shell-initialize)
  :if (not (memq system-type '(cygwin windows-nt)))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

;; Setup garbage collection
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Load everything in the config directory
(when (file-exists-p config-dir)
  (mapc (lambda (file)
          (load file nil nil t))
        (directory-files config-dir 't "^[^#\.].*\\.el$")))

(provide 'init)
;;; init.el ends here
