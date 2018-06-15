;;; misc.el --- Miscellaneous configurations          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat <mbax4tm2@E-C07KI1803.it.manchester.ac.uk>
;; Keywords: local

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

;; Load the theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/personal/themes")
(load-theme 'cyan t)

;; Use an in-emacs gpg agent
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; Display the time in the mode line
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (setq display-time-default-load-average nil)
;; (display-time-mode t)

;; Beacon
(setq beacon-color "#245361")

;; Dired
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-dwim-target t)

;; Default dictionary
(setq ispell-dictionary "en_US")

;; Tramp ssh
(setq tramp-default-method "scp")

;; Disable auto-save
(setq prelude-auto-save t)

;; Set autocompletion delay
(setq company-idle-delay 0.1)

;; Set default browser

(setq browse-url-browser-function 'browse-url-firefox)

;; Disable the annoying suspend-frame command
(put 'suspend-frame 'disabled t)

(provide 'misc)
;;; misc.el ends here
