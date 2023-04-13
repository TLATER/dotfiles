;;; features.el --- Configuration for additional non-text editing features      -*- lexical-binding: t; -*-

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
;; Render emoji
;; ----------------------------------------------------------------------------------

(when (member "Noto Emoji" (font-family-list))
  (set-fontset-font t 'unicode "Noto Emoji" nil 'prepend))

;; ----------------------------------------------------------------------------------
;;; In-emacs terminal
;; ----------------------------------------------------------------------------------

(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (set (make-local-variable 'global-hl-line-mode) nil)))
  :bind (:map vterm-mode-map
              ("C-c C-j" . vterm-copy-mode))
  :custom-face
  (vterm-color-black ((t (:foreground "#0f0f0f" :background "#707880"))))
  :config
  (add-to-list 'vterm-eval-cmds '("magit" magit-status))
  (add-to-list 'vterm-eval-cmds '("woman" woman)))

;; ----------------------------------------------------------------------------------
;; Startup dashboard
;; ----------------------------------------------------------------------------------

(use-package dashboard
  :demand
  :commands dashboard-setup-startup-hook
  :custom
  (dashboard-set-init-info t)
  (dashboard-set-footer nil)
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

;; ----------------------------------------------------------------------------------
;; Git porcelain
;; ----------------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------------
;; Much improved prompts
;; ----------------------------------------------------------------------------------

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

(use-package helm-rg
  :after projectile)

(use-package helm-projectile
  :functions helm-projectile-on
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
  :init
  (declare-function helm-projectile-on nil)
  :config
  (helm-projectile-on))

;; ----------------------------------------------------------------------------------
;; Non-man documentation browser
;; ----------------------------------------------------------------------------------

;; TODO(tlater): Figure out how to preinstall docsets
(use-package dash-docs
  :functions dash-docs-activate-docset
  :hook
  (python-mode-hook . (lambda () (dash-docs-activate-docset "Python 3")))
  :custom
  (dash-docs-docsets-path (expand-file-name "docsets" data-dir))
  (dash-docs-browser-func 'eww-browse-url)
  :init
  (declare-function dash-docs-activate-docset nil))
(use-package helm-dash
  :after (helm dash-docs)
  :bind
  ("C-c h d" . helm-dash))

;; ----------------------------------------------------------------------------------
;; Project management
;; ----------------------------------------------------------------------------------

(use-package projectile
  :functions projectile-discover-projects-in-search-path
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-use-git-grep t)
  (projectile-known-projects-file
   (expand-file-name "projectile-bookmarks.eld" data-dir))
  :config
  (projectile-discover-projects-in-search-path))

;; ----------------------------------------------------------------------------------
;; More mode-line info
;; ----------------------------------------------------------------------------------

(use-package smart-mode-line
  :functions (sml/setup sml/faces-from-theme sml/theme-p)
  :custom
  (sml/no-confirm-load-theme t)
  :init
  (declare-function sml/setup nil)
  :config
  (sml/setup))

;; ----------------------------------------------------------------------------------
;; Version control hints
;; ----------------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------------
;; Nicer HTML rendering
;; ----------------------------------------------------------------------------------

(use-package shr
  :custom
  (shr-use-colors nil))

;; ----------------------------------------------------------------------------------
;; Directory listings
;; ----------------------------------------------------------------------------------

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


(defconst typescript-tsc-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\),\\([0-9]+\\)):[[:blank:]]+"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

(defconst typescript-tsc-pretty-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\):\\([0-9]+\\):\\([0-9]+\\) - [[:blank:]]*"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;; ----------------------------------------------------------------------------------
;; Run various commands
;; ----------------------------------------------------------------------------------

;; Ensure proper ANSI code handling
(use-package xterm-color
  :after compile
  :functions (xterm-color-filter compilation-filter)
  :init
  (declare-function xterm-color-filter "xterm-color")
  (setq compilation-environment '("TERM=xterm-256color"))
  :config
  (declare-function compilation-filter@advice-compilation-filter nil)
  (define-advice compilation-filter
      (:around (orig proc string) advice-compilation-filter)
    (funcall orig proc (xterm-color-filter string))))

(use-package compile
  :after alert
  :custom
  (compilation-finish-functions
   (append compilation-finish-functions
           (lambda (_ status)
             (alert status
                    :title "Compilation finished"
                    :id 'emacs-compilation
                    :category 'compilation.complete))))
  (compilation-scroll-output t)
  :config
  ;; Make sure typescript output is handled correctly in compilation buffers
  (dolist
      (regexp
       `((typescript-tsc
          ,typescript-tsc-error-regexp
          1 2 3 2)

         (typescript-tsc-pretty
          ,typescript-tsc-pretty-error-regexp
          1 2 3 2)))
    (add-to-list 'compilation-error-regexp-alist-alist regexp)
    (add-to-list 'compilation-error-regexp-alist (car regexp))))

;;; features.el ends here
