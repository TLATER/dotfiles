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
  (require 'leaf))

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

(leaf crux
  :ensure t
  :bind
  ("C-c o" . crux-open-with)
  ([remap kill-line] . crux-smart-kill-line)
  ("C-c u" . crux-view-url)
  ("C-c e" . crux-eval-and-replace)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c C" . crux-copy-file-preserve-attributes)
  ("C-c R" . crux-rename-file-and-buffer)
  ([remap delete-indentation] . crux-top-join-line)
  :global-minor-mode crux-reopen-as-root-mode)

(leaf consult
  :ensure t
  :defun (consult-register-window consult-xref consult-register-format)
  :custom
  (register-preview-delay . 0.5)
  (register-preview-function . #'consult-register-format)
  (xref-show-xrefs-function . #'consult-xref)
  (xref-show-definitions-function . #'consult-xref)

  (consult-buffer-filter .
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
         ("M-s h" . consult-isearch-history)
         ;; Consider keep-lines, focus-lines
         )
  :advice (:override register-preview consult-register-window))

(leaf consult-eglot
  :ensure t
  :after eglot
  :bind ("M-s e" . consult-eglot-symbols))

(leaf consult-org-roam
  :ensure t
  :after org-roam
  :bind
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n s" . consult-org-roam-search)
  :custom
  (consult-org-roam-grep-func . #'consult-ripgrep)
  (consult-org-roam-buffer-after-buffers . t)
  :global-minor-mode consult-org-roam-mode)

(leaf consult-project-extra
  :ensure t
  :bind
  ([remap project-find-file] . consult-project-extra-find)
  ("C-x p 4 f" . consult-project-extra-find-other-window))

;; ----------------------------------------------------------------------------------
;; Highlights
;; ----------------------------------------------------------------------------------

;; Make pairs of parens highlight
(leaf paren
  :global-minor-mode show-paren-mode)

;; Highlight TODO notes
(leaf fic-mode
  :ensure t
  :hook prog-mode-hook
  :custom-face
  (fic-face . '((t (:foreground "darkred" :weight bold))))
  (fic-author-face . '((t (:foreground "orangered" :underline t)))))

;; Colorful color names :3
(leaf rainbow-mode
  :ensure t
  :hook (prog-mode-hook text-mode-hook))

;; Highlight the current line
(leaf hl-line
  :global-minor-mode global-hl-line-mode)

;; Show whitespace when it isn't correct
(leaf whitespace
  :custom
  (whitespace-style . '(face trailing indentation space-after-tab
                           space-before-tab))
  (whitespace-global-modes . '(not erc-mode))
  :global-minor-mode global-whitespace-mode)

;;; ui.el ends here
