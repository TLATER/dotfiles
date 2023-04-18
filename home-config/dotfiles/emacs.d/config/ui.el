;;; ui.el --- UI customization      -*- lexical-binding: t; -*-

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
  (require 'use-package))

;; ----------------------------------------------------------------------------------
;;; Custom navigation and text editing keybinds
;; ----------------------------------------------------------------------------------

(defun other-window-backwards ()
  "Go to the other window in reverse order."
  (interactive)
  (other-window -1))

(define-key global-map (kbd "C-x O") 'other-window-backwards)

(define-key global-map (kbd "<XF86Back>") nil)
(define-key global-map (kbd "<XF86Forward>") nil)

;; ----------------------------------------------------------------------------------
;;; Unbind keys I don't like
;; ----------------------------------------------------------------------------------

;; Remove undo keybindings; Undoing is handled using vundo instead
(define-key global-map (kbd "C-/") nil)
(define-key global-map (kbd "C-_") nil)
(define-key global-map (kbd "C-?") nil)

;; ----------------------------------------------------------------------------------
;;; Other various useful keybindings
;; ----------------------------------------------------------------------------------

(use-package crux
  :demand
  :functions (crux-reopen-as-root-mode)
  :bind
  ("C-c o" . crux-open-with)
  ([remap kill-line] . crux-smart-kill-line)
  ("C-c u" . crux-view-url)
  ("C-c e" . crux-eval-and-replace)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c C" . crux-copy-file-preserve-attributes)
  ("C-c R" . crux-rename-file-and-buffer)
  ([remap delete-indentation] . crux-top-join-line)
  :config
  (crux-with-region-or-buffer comment-or-uncomment-region)
  (crux-reopen-as-root-mode))

;; ----------------------------------------------------------------------------------
;; Highlights
;; ----------------------------------------------------------------------------------

;; Make pairs of parens highlight
(use-package paren
  :config
  (show-paren-mode 1))

;; Highlight TODO notes
(use-package fic-mode
  :hook prog-mode
  :custom-face
  (fic-face ((t (:foreground "darkred" :weight bold))))
  (fic-author-face ((t (:foreground "orangered" :underline t)))))

;; Colorful color names :3
(use-package rainbow-mode
  :hook (prog-mode text-mode))

;; Highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode 1))

;; Show whitespace when it isn't correct
(use-package whitespace
  :demand
  :commands global-whitespace-mode
  :custom
  (whitespace-style '(face trailing indentation space-after-tab
                           space-before-tab tab-mark))
  (whitespace-global-modes '(not erc-mode))
  :config
  (global-whitespace-mode))

;;; ui.el ends here
