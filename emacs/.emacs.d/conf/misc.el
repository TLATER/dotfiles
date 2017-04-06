;; ------------------------------------------------------------
;; misc.el
;;
;; Miscellaneous settings
;; ------------------------------------------------------------

;; Disable help messages
(setq show-help-function nil)

;; Disable auto save and backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Disable the alarm bell
(setq ring-bell-function 'ignore)

;; Remove the splash screen
(setq inhibit-startup-message t)

;; Disable other additional UI settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Set the email address
(setq user-mail-address "tm@tlater.net")
(setq user-full-name "Tristan Daniël Maat")

;; Enable dired improvements
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(setq dired-dwim-target t)

;; Add various simple packages
(use-package systemd)
(use-package undo-tree
  :config
  (global-undo-tree-mode))
