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
(defvar data-dir (expand-file-name
                  "emacs"
                  (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defvar back-dir (expand-file-name
                    "emacs/backups"
                    (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defvar share-dir (expand-file-name
                   "share"
                   (file-name-directory load-file-name)))
(setq custom-file (expand-file-name "custom.el" data-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set up package loading
(require 'package)
(setq package-user-dir (expand-file-name "elpa" data-dir))
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
(defvar using-external-packages (or (getenv "SCANNING_PACKAGES")
                                  (fboundp 'nix--profile-paths)))

;; Make sure use-package is installed if it's not installed externally
(unless using-external-packages
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                      (not (gnutls-available-p))))
         (proto (if no-ssl "http" "https")))
    (when no-ssl
      (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

  (unless package--initialized (package-initialize))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Setup use-package, regardless of how it is installed
;; bind-key is a dependency...
(require 'bind-key)
(require 'use-package)

;; If packages are installed externally, we want to turn "ensure" off
(setq use-package-always-ensure (not using-external-packages))
(setq use-package-compute-statistics t)
(when using-external-packages
  (setq use-package-ensure-function 'ignore)
  (setq package-enable-at-startup nil))

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
      backup-directory-alist `(("." . ,back-dir))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/" back-dir)
      auto-save-file-name-transforms `((".*/\\(.*\\)" ,(expand-file-name "\\1" back-dir) t)))
(setq create-lockfiles nil)

;; Might not want to do the following if we're not in a normal
;; environment, i.e., if we're *not* invoked as a daemon. Maybe set
;; some minimal components for daemon mode and stop here if we're
;; root?

;; Set the theme
(use-package gotham-theme
  :ensure t
  :load-path "themes"
  :init
  (setq gotham-tty-256-colors t)
  (setq gotham-tty-16-colors t)

  (defun gotham-pick-color (color-256 color-16 color-8)
    (if gotham-tty-256-colors color-256 (if gotham-tty-16-colors color-16 color-8)))

  (setq gotham-color-alist
  `((base0   "#0f0f0f" ,(gotham-pick-color "color-232" "black"        "black"))
    (base1   "#11151c" ,(gotham-pick-color "color-233" "brightblack"  "black"))
    (base2   "#091f2e" ,(gotham-pick-color "color-17"  "brightgreen"  "black"))
    (base3   "#0a3749" ,(gotham-pick-color "color-18"  "brightblue"   "blue"))
    (base4   "#245361" ,(gotham-pick-color "color-24"  "brightyellow" "cyan"))
    (base5   "#268bd2" ,(gotham-pick-color "color-81"  "brightcyan"   "cyan"))
    (base6   "#99d1ce" ,(gotham-pick-color "color-122" "white"        "white"))
    (base7   "#d3ebe9" ,(gotham-pick-color "color-194" "brightwhite"  "white"))

    (red     "#dc322f" ,(gotham-pick-color "color-124" "red"           "red"))
    (orange  "#d26937" ,(gotham-pick-color "color-166" "brightred"     "yellow"))
    (yellow  "#b58900" ,(gotham-pick-color "color-214" "yellow"        "yellow"))
    (magenta "#707880" ,(gotham-pick-color "color-67"  "brightmagenta" "white"))
    (violet  "#4e5166" ,(gotham-pick-color "color-60"  "magenta"       "blue"))
    (blue    "#195466" ,(gotham-pick-color "color-24"  "blue"          "blue"))
    (cyan    "#599cab" ,(gotham-pick-color "color-44"  "cyan"          "cyan"))
    (green   "#2aa889" ,(gotham-pick-color "color-78"  "green"         "green"))))
  :config
  (load-theme 'gotham t))

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
