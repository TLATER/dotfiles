;;; misc.el --- Miscellaneous configuration          -*- lexical-binding: t; -*-

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

;; The *best* indentation format
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Ask if we want to add newlines to files
(setq require-final-newline "visit-save")

;; Make dired hide hidden files
(use-package dired
  :commands dired
  :ensure nil
  :init
  (setq dired-dwim-target t))

(use-package dired-x
  :ensure nil
  :after (dired)
  :init
  (setq-default dired-omit-files-p t)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

;; Backdrop behind current line
(use-package beacon
  :init
  (setq beacon-color "#245361"))

;; Spell checking
(use-package ispell
  :init
  (setq ispell-dictionary "en_US"))

;; Speed up tramp a bit
(setq tramp-default-method "scp")

;; Set default browser
(use-package browse-url
  :functions browse-url
  :init
  (setq browse-url-browser-function 'browse-url-default-browser))

;; Let org-babel handle gnuplot
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t)))

;; Show "wrong" whitespace
(use-package whitespace
  :init
  (setq whitespace-style '(face trailing tabs space-after-tab
                           space-before-tab tab-mark))
  :config
  (global-whitespace-mode))

;; Remove trailing whitespace upon save
(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(provide 'misc)
;;; misc.el ends here
