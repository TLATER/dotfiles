;;; sane-defaults.el --- Override some of the more insane default emacs settings      -*- lexical-binding: t; -*-

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

;;

;;; Code:

(eval-and-compile
  (require 'use-package)
  (defvar back-dir))

;; ----------------------------------------------------------------------------------
;;; Remove pointless UI components
;; ----------------------------------------------------------------------------------

(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(setq inhibit-startup-screen t)

;; ----------------------------------------------------------------------------------
;;; Improve prompts and remove noises
;; ----------------------------------------------------------------------------------

(setq ring-bell-function 'ignore)
(setq large-file-warning-threshold 100000000)
(fset 'yes-or-no-p 'y-or-n-p)

;; ----------------------------------------------------------------------------------
;;; Make fat-fingering C-z not freeze up emacs
;; ----------------------------------------------------------------------------------

(defun suspend-non-graphical-frame ()
  "Suspend-frame, but don't do it to my graphical windows.

   ORIG is the original function, ARGS the arguments passed to the invocation."
  (when (not (display-graphic-p))
    (suspend-frame)))

(define-key global-map [remap suspend-frame] 'suspend-non-graphical-frame)

;; ----------------------------------------------------------------------------------
;;; Fix the annoying way emacs handles backup/autosave/lock files by default
;; ----------------------------------------------------------------------------------

(setq backup-by-copying t
      backup-directory-alist `(("." . ,back-dir))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/" back-dir))
(setq auto-save-file-name-transforms
      `((".*/\\(.*\\)" ,(expand-file-name "\\1" back-dir) t)))
;; This is for when two users edit the same file at the same time... It's 2023, who does
;; that?
(setq create-lockfiles nil)

;; ----------------------------------------------------------------------------------
;;; Fix auto-completion completing everything lowercase
;; ----------------------------------------------------------------------------------

(use-package dabbrev
  :custom
  (dabbrev-case-fold-search nil))

;;; sane-defaults.el ends here
