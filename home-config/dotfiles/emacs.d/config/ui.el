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

;; Replace various goto-related keybindings
(define-key global-map (kbd "M-g TAB") nil)
(define-key global-map (kbd "M-g c") nil)

;; Replace various search-related keybindings
(define-key global-map (kbd "M-s .") nil)
(define-key global-map (kbd "M-s _") nil)
(define-key global-map (kbd "M-s w") nil)
(define-key global-map (kbd "M-s M-.") nil)
(define-key global-map (kbd "M-s M-w") nil)

;; ----------------------------------------------------------------------------------
;;; Other various useful keybindings
;; ----------------------------------------------------------------------------------

(use-package crux
  :demand
  :bind
  ("C-c o" . crux-open-with)
  ([remap kill-line] . crux-smart-kill-line)
  ("C-c u" . crux-view-url)
  ("C-c e" . crux-eval-and-replace)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c C" . crux-copy-file-preserve-attributes)
  ("C-c R" . crux-rename-file-and-buffer)
  ([remap delete-indentation] . crux-top-join-line)
  :preface
  (declare-function crux-reopen-as-root-mode "crux.el")
  :config
  (crux-with-region-or-buffer comment-or-uncomment-region)
  (crux-reopen-as-root-mode))

(use-package consult
  :functions (consult-register-window consult-xref consult-register-format)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  (consult-buffer-filter
   `(,(rx string-start " ")
     ,(rx string-start "*Completions*" string-end)
     ,(rx string-start "*Flymake log*" string-end)
     ,(rx string-start "*Semantic SymRef*" string-end)
     ,(rx string-start "*tramp/" (zero-or-more anything) "*" string-end)
     ,(rx string-start "*Async-native-compile-log*" string-end)
     ,(rx string-start "*direnv*" string-end)
     ,(rx string-start "magit-process: " (zero-or-more anything) string-end)))

  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ("M-y" . consult-yank-pop)

         ;; 'goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; 'search-map'
         ("M-s s" . consult-line)
         ("M-s M-s" . consult-line)
         ("M-s S" . consult-line-multi)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)
         ("M-s m" . consult-man)
         ("M-s e" . consult-isearch-history)
         ;; Consider keep-lines, focus-lines
         )
  :config
  (advice-add #'register-preview :override #'consult-register-window))

(use-package consult-eglot
  :commands consult-eglot-symbols)

(use-package consult-org-roam
  :demand
  :after org-roam
  :bind
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n s" . consult-org-roam-search)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-after-buffers t)
  :preface
  (declare-function consult-org-roam-mode "consult-org-roam.el")
  :config
  (consult-org-roam-mode 1))

(use-package consult-project-extra
  :bind
  ([remap project-find-file] . consult-project-extra-find)
  ("C-x p 4 f" . consult-project-extra-find-other-window))

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
