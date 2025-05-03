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
  (require 'leaf))

;; ----------------------------------------------------------------------------------
;; Render emoji
;; ----------------------------------------------------------------------------------

(when (member "Noto Emoji" (font-family-list))
  (set-fontset-font t 'unicode "Noto Emoji" nil 'prepend))

;; ----------------------------------------------------------------------------------
;;; In-emacs terminal
;; ----------------------------------------------------------------------------------

(leaf vterm
  :ensure t
  :hook (vterm-mode-hook . (lambda ()
                             (set (make-local-variable 'global-hl-line-mode) nil)))
  :bind (:vterm-mode-map
         ("C-c C-j" . vterm-copy-mode))
  :custom-face
  (vterm-color-black . '((t (:foreground "#0f0f0f" :background "#707880"))))
  :defvar vterm-eval-cmds
  :config
  (add-to-list 'vterm-eval-cmds '("magit" magit-status))
  (add-to-list 'vterm-eval-cmds '("woman" woman)))

;; ----------------------------------------------------------------------------------
;; Startup dashboard
;; ----------------------------------------------------------------------------------

(leaf dashboard
  :ensure t
  :custom
  (dashboard-set-init-info . t)
  (dashboard-set-footer . nil)
  (dashboard-projects-backend . 'project-el)
  (initial-buffer-choice . (lambda ()
                             (or
                              (get-buffer "*dashboard*")
                              (get-buffer "*scratch*"))))
  (dashboard-items . '((recents . 5)
                       (bookmarks . 5)
                       (projects . 5)
                       (registers . 5)))
  :defun dashboard-setup-startup-hook
  :config
  (dashboard-setup-startup-hook))

;; ----------------------------------------------------------------------------------
;; Git porcelain
;; ----------------------------------------------------------------------------------

(leaf magit
  :ensure t
  :bind
  ("C-c g s" . magit-status)
  (:project-prefix-map
   ("m" . magit-project-status)))
(leaf transient
  :ensure t
  :custom
  `(transient-levels-file . ,(expand-file-name "transient/levels.el" data-dir))
  `(transient-values-file . ,(expand-file-name "transient/values.el" data-dir))
  `(transient-history-file . ,(expand-file-name "transient/history.el" data-dir)))
(leaf magit-lfs
  :ensure t
  :after magit)
(leaf sqlite3
  :ensure t)                   ; Required for forge
(leaf forge
  :ensure t
  :after (magit sqlite3)
  :custom
  `(forge-database-file . ,(expand-file-name "forge-database.sqlite" data-dir))
  :defvar forge-alist
  :config
  (add-to-list
   'forge-alist '("gitlab.codethink.co.uk" "gitlab.codethink.co.uk/api/v4"
                  "gitlab.codethink.co.uk" forge-gitlab-repository)))

;; ----------------------------------------------------------------------------------
;; Show keymaps as the prefix is entered
;; ----------------------------------------------------------------------------------

(leaf which-key
  :ensure t
  :custom
  (which-key-idle-delay . 0.5)
  (which-key-popup-type . 'minibuffer)
  (which-key-sort-order . 'which-key-prefix-then-key-order)
  (which-key-show-prefix . 'left)
  (which-key-use-C-h-commands . t)
  (which-key-max-description-length . 60)
  (which-key-show-docstrings . t)
  :defun which-key-setup-side-window-bottom
  :config
  (which-key-setup-side-window-bottom)
  :global-minor-mode which-key-mode)

;; ----------------------------------------------------------------------------------
;; Much improved prompts
;; ----------------------------------------------------------------------------------

(leaf vertico
  :ensure t
  :custom
  (vertico-count . 15)
  :global-minor-mode vertico-mode)

;; Config to make minibuffer search (e.g. for find-file) show up
;; differently - this is what makes M-s r g show up in a separate
;; buffer, for example.
(leaf vertico-multiform
  :after vertico
  :custom
  (vertico-multiform-commands . '((consult-imenu buffer)
                                  (consult-imenu-multi buffer)
                                  (consult-project-extra-find buffer)))
  (vertico-multiform-categories . '((consult-grep buffer)
                                    (file (vertico-sort-function . vertico-sort-alpha))))
  :global-minor-mode vertico-multiform-mode)

(leaf savehist
  :custom
  `(savehist-file . ,(expand-file-name "history" data-dir))
  :global-minor-mode savehist-mode)

(leaf orderless
  :ensure t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles basic partial-completion)))))

(leaf marginalia
  :ensure t
  :global-minor-mode marginalia-mode )

(leaf embark
  :ensure t
  :defun embark--truncate-target
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :hook (eldoc-documentation-functions . embark-eldoc-first-target)
  :custom
  (prefix-help-command . 'embark-prefix-help-command)
  (embark-indicators . '(embark-which-key-indicator
                         embark-highlight-indicator
                         embark-isearch-highlight-indicator))
  :defun (embark-completing-read-prompter
          embark--truncate-target
          embark-which-key-indicator
          which-key--hide-popup-ignore-command
          which-key--show-keymap)
  :defvar embark-indicators
  :config
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
    (let ((embark-indicators (remq #'embark-which-key-indicator embark-indicators)))
      (ignore embark-indicators)
      (apply fn args)))

  :advice (:around embark-completing-read-prompter #'embark-hide-which-key-indicator))

(leaf embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(leaf emacs
  :hook (minibuffer-setup-hook . cursor-intangible-mode)
  :custom
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties . '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (read-extended-command-predicate . #'command-completion-default-include-p)
  :defvar crm-separator
  :config
  ;; https://github.com/minad/vertico/blob/main/README.org?plain=1#L112
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple' (silence ARGS warning).

    We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  :advice (:filter-args completing-read-multiple crm-indicator))

;; ----------------------------------------------------------------------------------
;; Non-man documentation browser
;; ----------------------------------------------------------------------------------

;; TODO(tlater): Figure out how to preinstall docsets
(leaf devdocs
  :ensure t
  :bind ("C-h D" . devdocs-lookup)
  :custom
  `(devdocs-data-dir . ,(expand-file-name "docsets" data-dir)))

;; ----------------------------------------------------------------------------------
;; Code coverage
;; ----------------------------------------------------------------------------------

(leaf cov
  :ensure t
  :hook (prog-mode-hook . cov-mode)
  :custom
  (cov-lcov-patterns . '((lambda (dir name)
                           (expand-file-name "lcov.info"
                                             (if (project-current nil dir)
                                                 (project-root (project-current t dir))
                                               dir)))))
  :custom-face
  (cov-none-face . '((t (:foreground "red"))))
  (cov-heavy-face . '((t (:foreground "green")))))

;; ----------------------------------------------------------------------------------
;; Project management
;; ----------------------------------------------------------------------------------

(leaf project
  :bind (:project-prefix-map
         ("f" . consult-project-extra-find)
         ("v" . nil)
         ("x" . nil)
         ("s" . nil)
         ("e" . nil)
         ("x v" . project-vterm)
         ("x e" . project-eshell)
         ("x s" . project-shell))
  :custom
  `(project-list-file . ,(expand-file-name "projects" data-dir))
  :defun project-root project-prefixed-buffer-name vterm
  :config
  (require 'vterm)

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

(leaf smart-mode-line
  :ensure t
  :defun (sml/setup sml/faces-from-theme sml/theme-p)
  :custom
  (sml/theme . 'respectful)
  (sml/replacer-regexp-list . `((,(rx string-start "~/Documents/Projects/") ":PRJ:")
                                (,(rx string-start "~/.local/src/dotfiles/") ":DTF:")
                                (,(rx string-start "/sudo:" (zero-or-more anything) ":") ":SU:")))
  :config
  (sml/setup))

(leaf rich-minority
  :ensure t
  :custom
  `(rm-blacklist . ,(rx
                     (or
                      "ARev"
                      "EditorConfig"
                      (and "Fly/" (zero-or-more anything))
                      "cov"
                      "Rbow"
                      "company"
                      "cor"
                      "WK"
                      "SP"
                      "yas"
                      "WSC"
                      "WS"
                      "GCMH"
                      "Eldoc"))))

;; ----------------------------------------------------------------------------------
;; Version control hints
;; ----------------------------------------------------------------------------------

(leaf diff-hl
  :ensure t
  :demand
  :commands diff-hl-flydiff-mode global-diff-hl-mode
  :custom-face
  (diff-hl-insert . '((t (:inherit nil :foreground unspecified :background "#2aa889"))))
  (diff-hl-delete . '((t (:inherit nil :foreground unspecified :background "#dc322f"))))
  (diff-hl-change . '((t (:inherit nil :foreground unspecified :background "#d26937"))))
  :custom
  (diff-hl-draw-borders . nil)
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :global-minor-mode global-diff-hl-mode diff-hl-flydiff-mode)

;; ----------------------------------------------------------------------------------
;; Nicer HTML rendering
;; ----------------------------------------------------------------------------------

(leaf shr
  :custom
  (shr-use-colors . nil))

;; ----------------------------------------------------------------------------------
;; Directory listings
;; ----------------------------------------------------------------------------------

(leaf dired
  :commands dired
  :bind (("C-x d" . dired)
         (:dired-mode-map
          ("e" . dired-find-alternate-file)
          ("f" . dired-find-alternate-file)
          ("^" . (lambda () (interactive) (find-alternate-file "..")))))
  :custom
  (dired-dwim-target . t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(leaf dired-hacks-utils
  :ensure t
  :after dired
  :hook (dired-mode-hook . dired-utils-format-information-line-mode))
(leaf dired-filter
  :ensure t
  :after dired
  :hook (dired-mode-hook . dired-filter-mode)
  :custom
  (dired-filter-stack . '((dot-files) (git-ignored)))
  :bind-keymap
  (:dired-mode-map
   ("C-c f" . dired-filter-map)))
(leaf dired-subtree
  :ensure t
  :after dired
  :bind (:dired-mode-map
         :package dired
         ("RET" . dired-subtree-insert)
         ("i" . dired-subtree-insert)
         ("$" . dired-subtree-remove)
         ("C-<up>" . dired-subtree-previous-sibling)
         ("C-<down>" . dired-subtree-next-sibling)))
(leaf dired-narrow
  :ensure t
  :after dired
  :bind (:dired-mode-map
         :package dired
         ("C-s" . dired-narrow)))
(leaf dired-collapse
  :ensure t
  :after dired
  :hook (dired-mode-hook . dired-collapse-mode))


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
(leaf restclient
  :ensure t)
(leaf restclient-jq
  :ensure t
  :after restclient)

;; ----------------------------------------------------------------------------------
;; Run various commands
;; ----------------------------------------------------------------------------------

;; Ensure proper ANSI code handling
(leaf fancy-compilation
  :ensure t
  :after compile
  :custom
  (fancy-compilation-override-colors . nil)
  :global-minor-mode t)

(leaf alert
  :ensure t
  :commands alert
  :custom
  (alert-default-style . 'libnotify))

(leaf compile
  :custom
  (compilation-scroll-output . t)
  :defvar (compilation-error-regexp-alist compilation-error-regexp-alist-alist compilation-finish-functions)
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
