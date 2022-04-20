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

(eval-and-compile
  (require 'use-package))

;; The *best* indentation format
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Stop prompting whether we really want to stop when we have a
;; running process (this just clogs up `systemctl --user stop emacs`
;; without ever being useful, since emacs is running as a daemon
;; anyway).
(setq confirm-kill-processes 'nil)

(use-package auth-source
  :after auth-source-pass
  :functions auth-sources
  :ensure nil
  :custom
  ;; Sadly, despite the general use of password-store, znc wants me to
  ;; prepend some things to my IRC password, so I can't pull it from
  ;; there without allowing partial plaintext attacks.
  ;;
  ;; There are also some passwords that are inherently
  ;; machine-specific, which are best defined inside an authinfo file.
  ;;
  ;; Hence, add an authinfo file for those passwords, and give it
  ;; priority so we can override the general matches.
  (auth-sources '("~/.local/share/authinfo.gpg" password-store)))

(use-package auth-source-pass
  :ensure nil
  :custom
  (auth-source-pass-filename "~/.local/share/password-store")
  :config
  (auth-source-pass-enable))

(use-package recentf
  :ensure nil
  :demand
  :commands recentf-save-list
  :custom
  (recentf-save-file (expand-file-name "recentf" data-dir))
  :config
  (add-hook 'delete-frame-functions (lambda (_) (recentf-save-list))))

(use-package tramp
  :ensure nil
  :custom
  (tramp-persistency-file-name (expand-file-name "tramp" data-dir))
  (tramp-default-method "scp"))

(use-package url
  :ensure nil
  :custom
  (url-configuration-directory (expand-file-name "url" data-dir)))

;; Ask if we want to add newlines to files
(use-package files
  :ensure nil
  :custom
  (require-final-newline "visit-save"))

;; Set user name/email
(setq user-full-name "Tristan Daniël Maat")
(setq user-mail-address "tm@tlater.net")

(use-package message
  :ensure nil
  :custom
  (message-send-mail-function 'message-send-mail-with-sendmail))

(use-package sendmail
  :ensure nil
  :command sendmail-send-it
  :custom
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp"))

;; Make dired hide hidden files
(use-package dired
  :commands dired
  :ensure nil
  :bind (("C-x d" . dired)
         :map dired-mode-map
         ("e" . dired-find-alternate-file)
         ("f" . dired-find-alternate-file)
         ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  :custom
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-hacks-utils
  :after dired
  :hook (dired-mode . dired-utils-format-information-line-mode))
(use-package dired-filter
  :after dired
  :hook (dired-mode . dired-filter-mode)
  :custom
  (dired-filter-stack '((dot-files) (git-ignored)))
  :config
  ;; Work around https://github.com/jwiegley/use-package/issues/586
  (define-key dired-mode-map (kbd "C-c f") dired-filter-map))
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("RET" . dired-subtree-insert)
              ("i" . dired-subtree-insert)
              ("$" . dired-subtree-remove)
              ("C-<up>" . dired-subtree-previous-sibling)
              ("C-<down>" . dired-subtree-next-sibling)))
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("C-s" . dired-narrow)))
(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

;; Set default browser
(use-package browse-url
  :commands browse-url
  :custom
  (browse-url-browser-function 'browse-url-default-browser))

;; Configure org-mode
(use-package org
  :functions org-babel-do-load-languages
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-latex-listings t "Whether to use lstlistings for org latex exports")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t))))

(use-package org-ref
  :after org)

;; Show "wrong" whitespace
(use-package whitespace
  :demand
  :commands global-whitespace-mode
  :custom
  (whitespace-style '(face trailing indentation space-after-tab
                           space-before-tab tab-mark))
  (whitespace-global-modes '(not erc-mode))
  :config
  (global-whitespace-mode))

;; Remove trailing whitespace upon save
(use-package whitespace-cleanup-mode
  :demand
  :commands global-whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

(use-package alert
  :commands alert
  :custom
  (alert-default-style 'libnotify))

(use-package compile
  :after alert
  :ensure nil
  :custom
  (compilation-finish-functions
   (append compilation-finish-functions
           (lambda (_ status)
             (alert status
                    :title "Compilation finished"
                    :id 'emacs-compilation
                    :category 'compilation.complete))))
  (compilation-scroll-output t))

(when (member "Noto Emoji" (font-family-list))
  (set-fontset-font t 'unicode "Noto Emoji" nil 'prepend))

(provide 'misc)
;;; misc.el ends here
