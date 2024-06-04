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
  (dashboard-projects-backend 'project-el)
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
  :bind
  ("C-c g s" . magit-status)
  (:map project-prefix-map
        ("m" . magit-project-status)))
(use-package transient
  :custom
  (transient-levels-file (expand-file-name "transient/levels.el" data-dir))
  (transient-values-file (expand-file-name "transient/values.el" data-dir))
  (transient-history-file (expand-file-name "transient/history.el" data-dir)))
(use-package magit-lfs
  :after magit)
(use-package sqlite3)                   ; Required for forge
(use-package forge
  :after magit
  :custom
  (forge-database-file (expand-file-name "forge-database.sqlite" data-dir))
  :config
  (add-to-list
   'forge-alist '("gitlab.codethink.co.uk" "gitlab.codethink.co.uk/api/v4"
                  "gitlab.codethink.co.uk" forge-gitlab-repository)))

;; ----------------------------------------------------------------------------------
;; Show keymaps as the prefix is entered
;; ----------------------------------------------------------------------------------

(use-package which-key
  :demand
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'minibuffer)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-show-prefix 'left)
  (which-key-use-C-h-commands t)
  (which-key-max-description-length 60)
  (which-key-show-docstrings t)
  :preface
  (declare-function which-key-setup-side-window-bottom "which-key.el")
  (declare-function which-key-mode "which-key.el")
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;; ----------------------------------------------------------------------------------
;; Much improved prompts
;; ----------------------------------------------------------------------------------

(use-package vertico
  :demand
  :custom
  (vertico-count 15)
  (vertico-multiform-commands '((consult-imenu buffer)
                                (consult-imenu-multi buffer)))
  (vertico-multiform-categories '((consult-grep buffer)))
  :preface
  (declare-function vertico-mode "vertico.el")
  (declare-function vertico-multiform-mode "vertico-multiform.el")
  :config
  (require 'vertico-multiform)
  (vertico-mode 1)
  (vertico-multiform-mode 1))

(use-package savehist
  :demand
  :custom
  (savehist-file (expand-file-name "history" data-dir))
  :config
  (savehist-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand
  :preface
  (declare-function marginalia-mode "marginalia.el")
  :config
  (marginalia-mode 1))

(use-package embark
  :functions embark--truncate-target
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :hook (eldoc-documentation-functions . embark-eldoc-first-target)
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :preface
  (declare-function embark-completing-read-prompter "embark.el")
  (declare-function embark--truncate-target "embark.el")
  (declare-function which-key--hide-popup-ignore-command "which-key.el")
  (declare-function which-key--show-keymap "which-key.el")


  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.

   The which-key help message will show the type and value of the
   current target followed by an ellipsis if there are further
   targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Alternate which-key based indicator.

     Hide the which-key indicator immediately when using the
     \\='completing-read\\=' prompter.  FN ARGS."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  :config
  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package emacs
  :hook (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; Add prompt indicator to `completing-read-multiple'.
;; We display [select-multiple<separator>], e.g., [select-multiple,]
;; if the separator is a comma.
(define-advice completing-read-multiple
    (:around (&rest args) crm-indicator)
  (cons (format "[select-multiple%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

;; ----------------------------------------------------------------------------------
;; Non-man documentation browser
;; ----------------------------------------------------------------------------------

;; TODO(tlater): Figure out how to preinstall docsets
(use-package devdocs
  :bind ("C-h D" . devdocs-lookup)
  :custom
  (devdocs-data-dir (expand-file-name "docsets" data-dir)))

;; ----------------------------------------------------------------------------------
;; Project management
;; ----------------------------------------------------------------------------------

(use-package project
  :bind (:map project-prefix-map
         ("f" . consult-project-extra-find)
         ("v" . nil)
         ("x" . nil)
         ("s" . nil)
         ("e" . nil)
         ("x v" . project-vterm)
         ("x e" . project-eshell)
         ("x s" . project-shell))
  :custom
  (project-list-file (expand-file-name "projects" data-dir))
  :preface
  (declare-function project-root "project.el")
  (declare-function project-prefixed-buffer-name "project.el")
  :config
  (require 'vterm)
  (declare-function vterm "vterm.el")

  (defun project-vterm ()
    "Open or switch to a vterm buffer for the current project.

     If the prefix ARG is set, open another vterm buffer."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer default-project-vterm-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm (generate-new-buffer-name default-project-vterm-name))))))

;; ----------------------------------------------------------------------------------
;; More mode-line info
;; ----------------------------------------------------------------------------------

(use-package smart-mode-line
  :functions (sml/setup sml/faces-from-theme sml/theme-p)
  :custom
  (sml/no-confirm-load-theme t)
  :preface
  (declare-function sml/setup "smart-mode-line")
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
;;
;; REST browsing
;;
(use-package restclient)
(use-package restclient-jq
  :after restclient)

;; ----------------------------------------------------------------------------------
;; Run various commands
;; ----------------------------------------------------------------------------------

;; Ensure proper ANSI code handling
(use-package xterm-color
  :after compile
  :functions (xterm-color-filter compilation-filter)
  :preface
  (declare-function xterm-color-filter "xterm-color.el")
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  :config
  (declare-function compilation-filter@advice-compilation-filter "features.el")
  (define-advice compilation-filter
      (:around (orig proc string) advice-compilation-filter)
    (funcall orig proc (xterm-color-filter string))))

(use-package compile
  :commands (alert)
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
    (add-to-list 'compilation-error-regexp-alist (car regexp))
    (add-to-list 'compilation-finish-functions (lambda (_ status)
                                                 (pcase status
                                                   ('interrupted nil)
                                                   (status
                                                    (alert status
                                                           :title "Compilation finished"
                                                           :id 'emacs-completion
                                                           :category 'compilation.complete)))))))

;;; features.el ends here
