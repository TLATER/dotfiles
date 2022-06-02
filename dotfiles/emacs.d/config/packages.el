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
(defvar using-external-packages)
(defvar share-dir)
(eval-and-compile
  (require 'use-package))

(use-package fic-mode
  :hook prog-mode
  :custom-face
  (fic-face ((t (:foreground "darkred" :weight bold))))
  (fic-author-face ((t (:foreground "orangered" :underline t)))))

(use-package separedit
  :commands #'separedit
  :bind (:map prog-mode-map
              ("C-c ;" . #'separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-preserve-string-indentation t))

(use-package dashboard
  :demand
  :commands dashboard-setup-startup-hook
  :custom
  (initial-buffer-choice (lambda ()
                           (or
                            (get-buffer "*dashboard*")
                            (get-buffer "*scratch*"))))
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

;; Use direnv variables for projects
(use-package direnv
  :demand
  :commands direnv-mode
  :config
  (direnv-mode))

;; Automatically update packages every once in a while
(use-package auto-package-update
  :defer 20
  :unless using-external-packages
  :commands auto-package-update-maybe
  :custom
  (auto-package-update-hide-results t)
  (auto-package-update-last-update-day-path
   (expand-file-name ".update-day" data-dir))
  :config
  (auto-package-update-maybe)
  (add-hook 'auto-package-update-before-hook
            (lambda () (package-refresh-contents))))

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
  :custom
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" back-dir))))
  :hook (after-init . global-undo-tree-mode))

;; Auto-completion
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1))

;; Spell checking
(use-package flyspell
  :ensure nil
  :bind (:map flyspell-mode-map
              ("C-;" . nil))
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))
(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-dictionary "en_US"))

;; Better parens handling
(use-package smartparens
  :defer 5
  :functions (smartparens-global-mode sp-use-smartparens-bindings)
  :bind (:map smartparens-mode-map
              ("C-S-a" . sp-beginning-of-sexp)
              ("C-S-e" . sp-end-of-sexp)
              ("M-<left>" . sp-backward-up-sexp)
              ("M-<right>" . sp-down-sexp)
              ("M-<down>" . sp-forward-sexp)
              ("M-<up>" . sp-backward-sexp))
  :custom
  (sp-navigate-interactive-always-progress-point t)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings))

;; Magit!
(use-package magit
  :bind ("C-c g s" . magit-status))
(use-package transient
  :custom
  (transient-levels-file (expand-file-name "transient/levels.el" data-dir))
  (transient-values-file (expand-file-name "transient/values.el" data-dir))
  (transient-history-file (expand-file-name "transient/history.el" data-dir)))
(use-package magit-lfs
  :after magit)
(use-package forge
  :after magit
  :custom
  (forge-database-file (expand-file-name "forge-database.sqlite" data-dir))
  :config
  (add-to-list
   'forge-alist '("gitlab.codethink.co.uk" "gitlab.codethink.co.uk/api/v4"
                  "gitlab.codethink.co.uk" forge-gitlab-repository)))

;; Better prompts
(use-package helm
  :demand
  :commands helm-mode
  :defines helm-boring-buffer-regexp-list
  :init
  (add-hook 'helm-mode-hook
            (lambda ()
              (setq completion-styles
                    (cond ((assq 'helm-flex completion-styles-alist)
                           '(helm-flex))
                          ((assq 'flex completion-styles-alist)
                           '(flex))))))
  :config
  (require 'helm-config)
  (helm-mode 1)
  :custom
  (helm-ff-lynx-style-map t)
  (helm-completion-style 'emacs)
  :bind
  ([remap execute-extended-command] . helm-M-x)
  ([remap find-file] . helm-find-files)
  ([remap insert-char] . helm-ucs)
  ([remap apropos] . helm-apropos)
  ([remap switch-to-buffer] . helm-buffers-list)
  ("C-x c" . nil) ; Unbind command map; I prefer doing my own thing
  ("C-c h i" . helm-imenu)
  ("C-c h s" . helm-occur)
  ("C-c h u" . helm-ucs) ; Insert UTF-8 character
  ("C-c h f" . helm-for-files)
  ("C-c h b" . helm-bookmarks)
  ("C-c h m" . helm-mark-ring)
  :config
  (setq helm-boring-buffer-regexp-list
        (append helm-boring-buffer-regexp-list
                '("magit-.*:.*" "*Flymake diagnostics*"))))

(use-package helm-tramp
  :after helm
  :bind
  ("C-c h t" . helm-tramp))

;; Doc browser
(use-package dash-docs
  :functions dash-docs-activate-docset
  :hook
  (python-mode-hook . (lambda () (dash-docs-activate-docset "Python 3")))
  :custom
  (dash-docs-docsets-path (expand-file-name "docsets" data-dir))
  (dash-docs-browser-func 'eww-browse-url))
(use-package helm-dash
  :after (helm dash-docs)
  :bind
  ("C-c h d" . helm-dash))

;; A nice UI for ripgrep when not used with projectile
(use-package deadgrep
  :commands deadgrep)

;; Project browser
(use-package projectile
  :functions (projectile-discover-projects-in-search-path)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t)
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" data-dir))
  :config
  (projectile-discover-projects-in-search-path))

(use-package helm-rg
  :after projectile)

(use-package helm-projectile
  :functions (helm-projectile-on)
  :after (helm projectile)
  :bind
  ([remap projectile-find-other-file] .
   helm-projectile-find-other-file)
  ([remap projectile-find-file] .
   helm-projectile-find-file)
  ([remap projectile-find-file-in-known-projects] .
   helm-projectile-find-file-in-known-projects)
  ([remap projectile-find-file-dwim] .
   helm-projectile-find-file-dwim)
  ([remap projectile-find-dir] .
   helm-projectile-find-dir)
  ([remap projectile-switch-project] .
   helm-projectile-switch-project)
  ([remap projectile-recentf] .
   helm-projectile-recentf)
  ([remap projectile-ripgrep] .
   helm-projectile-rg)
  ([remap projectile-switch-to-buffer] .
   helm-projectile-switch-to-buffer)
  ([remap projectile-browse-dirty-projects] .
   helm-projectile-browse-dirty-projects)
  :config
  (helm-projectile-on))

;; Colorful color names :3
(use-package rainbow-mode
  :hook (prog-mode text-mode))

;; Better mode-line
(use-package smart-mode-line
  :functions (sml/setup sml/faces-from-theme sml/theme-p)
  :custom
  (sml/no-confirm-load-theme t)
  :config
  (sml/setup))

(use-package diff-hl
  :demand
  :commands diff-hl-flydiff-mode global-diff-hl-mode
  :custom-face
  (diff-hl-insert ((t (:inherit nil :foreground nil :background "#2aa889"))))
  (diff-hl-delete ((t (:inherit nil :foreground nil :background "#dc322f"))))
  (diff-hl-change ((t (:inherit nil :foreground nil :background "#d26937"))))
  :custom
  (diff-hl-draw-borders nil)
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

(use-package shr
  :custom
  (shr-use-colors nil))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode))

(use-package eshell
  :ensure nil
  :commands eshell
  :custom
  (eshell-directory-name (expand-file-name "eshell" data-dir)))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load))

(provide 'packages)
;;; packages.el ends here
