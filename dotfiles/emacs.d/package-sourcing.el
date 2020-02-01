;;; init.el --- Configures nix/use-package      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Tristan Daniël Maat

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

;; Figure out if package installation is handled externally.
;;
;; We do this by checking if a function defined by the nix site lisp
;; is bound.
(eval-and-compile
  (require 'package)
  (setq using-external-packages (or (getenv "SCANNING_PACKAGES")
                                    (fboundp 'nix--profile-paths)))

  ;; Make sure use-package is installed if it's not installed externally
  (unless using-external-packages
    (setq package-user-dir (expand-file-name "elpa" data-dir))

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
    (setq package-enable-at-startup nil)))

(provide 'package-sourcing)
;;; package-sourcing.el ends here
