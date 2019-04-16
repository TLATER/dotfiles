;;; packages.el --- Generic packages with little configuration  -*- lexical-binding: t; -*-

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

;; Automatically update packages every once in a while
(use-package auto-package-update
  :functions (auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Handy commands
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

;; Better undo
(use-package undo-tree
  :bind ([remap undo] . undo-tree-visualize))

;; Auto-completion
(use-package company
  :hook (after-init . global-company-mode)
  :init
  (setq company-idle-delay 0.1))

;; LSP integration
(use-package lsp-mode
  :hook rust-mode)
(use-package lsp-ui
  :after (lsp-mode))
(use-package company-lsp
  :after (lsp-mode company))

;; In-line linting
(use-package flycheck
  :functions (global-flycheck-mode)
  :config
  (global-flycheck-mode))

;; Better parens handling
(use-package smartparens
  :functions (smartparens-global-mode sp-use-smartparens-bindings)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings))

;; Magit!
(use-package magit
  :bind ("C-c g s" . magit-status))

;; Better prompts
(use-package helm
             :config
             (require 'helm-config)
             (helm-mode 1)
             :bind
             ([remap execute-extended-command] . helm-M-x)
             ([remap find-file] . helm-find-files))

;; Doc browser
(use-package helm-dash
  :after (helm)
  :bind
  ("C-c d" . helm-dash))

;; Project browser
(use-package projectile
  :functions (projectile-discover-projects-in-search-path)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/Documents/Projects/"))
  :config
  (projectile-discover-projects-in-search-path))
(use-package helm-projectile
  :functions (helm-projectile-on)
  :after (helm projectile)
  :bind
  ([remap projectile-find-other-file] . helm-projectile-find-other-file)
  ([remap projectile-find-file] . helm-projectile-find-file)
  ([remap projectile-find-file-in-known-projects] . helm-projectile-find-file-in-known-projects)
  ([remap projectile-find-file-dwim] . helm-projectile-find-file-dwim)
  ([remap projectile-find-dir] . helm-projectile-find-dir)
  ([remap projectile-switch-project] . helm-projectile-switch-project)
  ([remap projectile-recentf] . helm-projectile-recentf)
  ([remap projectile-switch-to-buffer] . helm-projectile-switch-to-buffer)
  ([remap projectile-grep] . helm-projectile-grep)
  ([remap projectile-ack] . helm-projectile-ack)
  ([remap projectile-ag] . helm-projectile-ag)
  ([remap projectile-ripgrep] . helm-projectile-rg)
  ([remap projectile-browse-dirty-projects] . helm-projectile-browse-dirty-projects)
  :config
  (helm-projectile-on))

;; Colorful color names :3
(use-package rainbow-mode)

;; Better mode-line
(use-package smart-mode-line
  :functions (sml/setup)
  :config
  (sml/setup))

;; Better snippets
(use-package yasnippet
  :functions (yas-global-mode)
  :commands yas-expand-snippet
  :config
  (yas-global-mode 1))

(use-package yatemplate
  :functions (yatemplate-fill-alist)
  :config
  (yatemplate-fill-alist))

(provide 'packages)
;;; packages.el ends here