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
(use-package files
  :ensure nil
  :init
  (setq require-final-newline "visit-save"))

;; Set user name/email
(setq user-full-name "Tristan Daniël Maat")
(setq user-mail-address "tm@tlater.net")

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
;; Don't use-package tramp here because I'm not sure what function
;; invokes tramp
(setq tramp-default-method "scp")

;; Set default browser
(use-package browse-url
  :commands browse-url
  :init
  (setq browse-url-browser-function 'browse-url-default-browser))

;; Configure org-mode
(use-package org
  :functions (org-babel-do-load-languages)
  :mode ("\\.org\\'" . org-mode)
  :init
  (defvar org-latex-listings t "Whether to use lstlistings for org latex exports")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t))))

(use-package org-ref
  :after (org))

;; Show "wrong" whitespace
(use-package whitespace
  :functions (global-whitespace-mode)
  :init
  (setq whitespace-style '(face trailing tabs space-after-tab
                           space-before-tab tab-mark))
  (setq whitespace-global-modes '(not erc-mode))
  :config
  (global-whitespace-mode))

;; Remove trailing whitespace upon save
(use-package whitespace-cleanup-mode
  :functions (global-whitespace-cleanup-mode)
  :config
  (global-whitespace-cleanup-mode))

(use-package compile
  :ensure nil
  :init
  (setq compilation-finish-functions
        (append compilation-finish-functions
                (lambda (buffer status)
                  (call-process "notify-send" nil nil nil
                                "-t" "0"
                                "-i" "emacs"
                                "Compilation finished in Emacs"
                                status))))
  (setq compilation-scroll-output t))

(provide 'misc)
;;; misc.el ends here
