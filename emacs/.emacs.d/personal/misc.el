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
(defun browse-url-vivaldi (url &optional _new-window)
  "Ask the Vivaldi WWW browser to load URL."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "vivaldi " url) nil
           "vivaldi"
           (append
            browse-url-chromium-arguments
            (list url)))))

(setq browse-url-browser-function 'browse-url-vivaldi)

;; Get rid of (suspend-frame) in graphical displays
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(provide 'misc)
;;; misc.el ends here
