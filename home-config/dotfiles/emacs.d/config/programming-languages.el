;;; programming-languages.el --- Configuration for specific programming languages      -*- lexical-binding: t; -*-

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
  (defvar config-dir)
  (defvar share-dir))

;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------
;;; Language mode configuration
;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------
;;; Bazel
;; ----------------------------------------------------------------------------------

(use-package bazel
  :mode (((rx ".bzl" string-end) . bazel-starlark-mode)
         ((rx (or "BUILD" "BUILD.bazel") string-end) . bazel-build-mode))
  :commands bazel-buildifier)

;; ----------------------------------------------------------------------------------
;;; CSV
;; ----------------------------------------------------------------------------------

(use-package csv-mode
  :mode (rx ".csv" string-end))

;; ----------------------------------------------------------------------------------
;;; Dockerfile
;; ----------------------------------------------------------------------------------

(use-package dockerfile-mode
  :mode (rx string-start "Dockerfile" string-end)
  :hook (dockerfile-mode . (lambda () (setq-local devdocs-current-docs '("docker")))))

;; ----------------------------------------------------------------------------------
;;; GLSL
;; ----------------------------------------------------------------------------------

(use-package glsl-mode
  :mode (rx (or ".glsl" ".vert" ".frag" ".geom") string-end))

;; ----------------------------------------------------------------------------------
;;; Gnuplot
;; ----------------------------------------------------------------------------------

(use-package gnuplot
  :mode ((rx (or ".p" ".gp" ".gnuplot") string-end) . gnuplot-mode)
  :hook (gnuplot-mode . (lambda () (setq-local devdocs-current-docs '("gnuplot")))))

;; ----------------------------------------------------------------------------------
;;; GraphQL
;; ----------------------------------------------------------------------------------

(use-package graphql-mode
  :mode (rx (or ".graphql" ".gql") string-end))

;; ----------------------------------------------------------------------------------
;;; Haskell
;; ----------------------------------------------------------------------------------

(use-package haskell-mode
  :commands haskell-mode-stylish-buffer
  :mode (rx ".hs" string-end)
  :hook (haskell-mode . interactive-haskell-mode)
  :hook (haskell-mode . (lambda () (setq-local devdocs-current-docs '("haskell~9"))))
  :bind (:map haskell-mode-map
              ("C-c `" . haskell-interactive-bring)))

;; ----------------------------------------------------------------------------------
;;; JSON & co.
;; ----------------------------------------------------------------------------------

(use-package json-mode
  :mode (rx ".json" string-end))

(use-package jsonnet-mode
  :mode (rx ".jsonnet" string-end))

(use-package yaml-mode
  :mode (rx (or ".yaml" ".yml" ".bst"
                (and string-start "project.conf")) string-end))

;; ----------------------------------------------------------------------------------
;;; Markdown
;; ----------------------------------------------------------------------------------

(use-package markdown-mode
  :hook (markdown-mode . (lambda () (setq-local devdocs-current-docs '("markdown"))))
  :mode (rx (or
             (and (or ".md" ".mdwn") string-end)
             (and string-start "/tmp/neomutt-")))
  :custom
  (markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; ----------------------------------------------------------------------------------
;;; nginx config files
;; ----------------------------------------------------------------------------------

(use-package nginx-mode
  :mode (rx "nginx.conf" string-end)
  :hook (markdown-mode . (lambda () (setq-local devdocs-current-docs '("nginx")))))

;; ----------------------------------------------------------------------------------
;;; Nix
;; ----------------------------------------------------------------------------------

(use-package nix-mode
  :commands nix-format-buffer
  :mode (rx ".nix" string-end)
  :hook (nix-mode . (lambda () (setq-local devdocs-current-docs '("nix")))))

;; ----------------------------------------------------------------------------------
;;; org
;; ----------------------------------------------------------------------------------

;; Configure org-mode
(use-package org
  :functions org-babel-do-load-languages
  :mode ((rx ".org" string-end) . org-mode)
  :custom
  (org-latex-listings t "Whether to use lstlistings for org latex exports")
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t))))

(use-package org-roam
  :demand
  :custom
  (org-roam-db-location (expand-file-name "org-roam.db" data-dir))
  (org-roam-directory "~/Documents/Notes/")
  (org-roam-dailies-directory "Journal") ; Relative to org-roam-directory
  (org-agenda-files '("~/Documents/Notes/Journal/"))
  :bind
  ("C-c n f" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :preface
  (declare-function org-roam-db-autosync-mode "org-roam.el")
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode 1))

;; Used by org-agenda to store the TODO mark
(use-package bookmark
  :custom
  (bookmark-file (expand-file-name "bookmarks" data-dir)))

;; ----------------------------------------------------------------------------------
;;; Protobuf
;; ----------------------------------------------------------------------------------

(use-package protobuf-mode
  :mode (rx ".proto" string-end))

;; ----------------------------------------------------------------------------------
;;; Python
;; ----------------------------------------------------------------------------------

(use-package python
  :mode ((rx ".py" string-end) . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.11"))))
  :init
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i"))
  (when (executable-find "ipython3")
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "--simple-prompt -i")))

(use-package cython-mode
  :mode ((rx (or ".pyx" ".pxd" ".pxi") string-end))
  :hook (cython-mode . (lambda () (setq-local devdocs-current-docs '("python~3.11")))))

;; ----------------------------------------------------------------------------------
;;; Rust
;; ----------------------------------------------------------------------------------

(use-package rust-mode
  :mode ((rx ".rs" string-end) . rust-mode)
  :hook (rust-mode . (lambda () (setq-local devdocs-current-docs '("rust")))))

(use-package cargo-mode
  :hook (rust-mode . (lambda () (setq-local devdocs-current-docs '("rust"))))
  :mode ((rx (or (and ".rs" string-end)
                 (and string-start "Cargo.toml" string-end)))
         . rust-mode))

;; ----------------------------------------------------------------------------------
;;; SCSS
;; ----------------------------------------------------------------------------------

(use-package scss-mode
  :mode (rx (or ".sass" ".scss") string-end)
  :hook (scss-mode . (lambda () (setq-local devdocs-current-docs '("css" "sass")))))

;; ----------------------------------------------------------------------------------
;;; *sh
;; ----------------------------------------------------------------------------------

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook (sh-mode . flymake-shellcheck-load)
  :hook (sh-mode . (lambda () (setq-local devdocs-current-docs '("bash")))))

;; ----------------------------------------------------------------------------------
;;; Systemd
;; ----------------------------------------------------------------------------------

(use-package systemd
  :mode ((rx (or".service" ".socket" ".device" ".mount" ".automount"
                ".swap" ".target" ".path" ".timer" ".slice" ".scope")
             string-end)
         . systemd-mode))

;; ----------------------------------------------------------------------------------
;;; TypeScript (and other web-related DSLs)
;; ----------------------------------------------------------------------------------

(use-package web-mode
  :bind
  :mode (rx (or ".pug" ".hbs" (and ".ts" (? "x"))) string-end)
  :hook (web-mode . (lambda () (setq-local devdocs-current-docs
                                           '("html"
                                             "typescript"
                                             "css"
                                             "javascript"
                                             "dom")))))

(use-package prettier-js
  :commands prettier-js)

;; ----------------------------------------------------------------------------------
;;; yuck - widget markup for eww
;; ----------------------------------------------------------------------------------

(use-package yuck-mode
  :mode (rx ".yuck" string-end))

;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------
;;; More generic programming-language support features
;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------
;;; Language servers
;; ----------------------------------------------------------------------------------

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.1))

(use-package eldoc
  :bind (("C-c l d" . 'eldoc-doc-buffer))
  :custom
    (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p .15)
  (eldoc-echo-area-display-truncation-message 'nil)
  :config
  (global-eldoc-mode 1))

(use-package xref
  :bind (("C-c l f" . xref-find-definitions-other-window)))

(defun set-eldoc-compose ()
  "Set documentation strategy to compose to work around flymake interference."
  ;; TODO(tlater): Find a nicer way to implement
  (with-suppressed-warnings ((obsolete eldoc-documentation-strategy))
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)))

(use-package eglot
  :commands (eglot eglot-format eglot-managed-p eglot--major-mode)
  :hook (((web-mode rust-mode python-mode sh-mode c-mode c++-mode nix-mode) .
          eglot-ensure)
         (eglot-managed-mode . set-eldoc-compose))
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l a" . eglot-code-actions)
        ("C-c l i" . consult-eglot-symbols))
  :init
  (setq eglot-workspace-configuration
        '((pylsp
           (plugins
            (pydocstyle
             (enabled . t)
             ;; Does not currently work:
             ;; https://github.com/python-lsp/python-lsp-server/issues/159
             (match . ".*"))
            (pycodestyle
             ;; Make compatible with black:
             ;; https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#line-length
             (maxLineLength . 88)
             ;; Only E203 is incompatible with black and enabled by
             ;; default, but defining *any* ignore list will override
             ;; the default ignore list, so we need to recreate the
             ;; original.
             ;;
             ;; See also the small caveat under the huge table here:
             ;; https://pycodestyle.pycqa.org/en/latest/intro.html#error-codes
             (ignore . ["E203" "E121" "E123" "E126" "E133" "E226"
                        "E241" "E242" "E704" "W503" "W504" "W505"]))))))

  :config
  (add-to-list 'eglot-server-programs
               '(rust-mode . ("rust-analyzer"
                              :initializationOptions
                              (:checkOnSave
                               (:command "clippy")))))
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil"))))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c l e" . flymake-show-buffer-diagnostics)))

;; ----------------------------------------------------------------------------------
;;; Autoformatting
;; ----------------------------------------------------------------------------------

(use-package reformatter
  :commands (alejandra-format-region
             alejandra-format-buffer
             clang-format-region
             clang-format-buffer
             latexindent-format-region
             latexindent-format-buffer)
  :config
  ;; Work around `make-variable-buffer-local' being called at a
  ;; non-top-level.
  ;;
  ;; Note: Do not do this without first checking if no other warnings
  ;; are thrown. In theory, we could use `with-suppressed-warnings',
  ;; but it doesn't look like there's any documentation on how to
  ;; disable this particular warning, and grepping the emacs source
  ;; for `make-variable-buffer-local' didn't yield anything useful.
  (with-no-warnings
    (reformatter-define alejandra-format
      :program "alejandra"
      :group 'nix-mode
      :lighter " AL")
    (reformatter-define clang-format
      :program "clang-format"
      :group 'glsl-mode
      :lighter " CF")
    (reformatter-define latexindent
      :program "latexindent"
      :group 'latex-mode
      :lighter " LF")))

(defcustom use-nixfmt nil
  "Use nixfmt for formatting nix instead of alejandra."
  :type 'boolean
  :local 'booleanp
  :group 'autoformat)

(defun autoformat ()
  "Autoformat the current buffer."
  (interactive)
  (pcase major-mode
    ('nix-mode (if use-nixfmt
                   (nix-format-buffer)
                 (alejandra-format-buffer)))
    ('glsl-mode
     (clang-format-buffer))
    ('latex-mode
     (latexindent-format-buffer))
    ((or 'mhtml-mode 'web-mode 'scss-mode)
     (prettier-js))
    ('haskell-mode (haskell-mode-stylish-buffer))
    ((or 'bazel-mode
         (app (lambda (m) (get m 'derived-mode-parent)) 'bazel-mode))
     (bazel-buildifier))
    (_ (if (eglot-managed-p)
           (eglot-format)
         (message "No formatter for this file type")))))

(define-key global-map (kbd "C-c f") 'autoformat)

;;; programming-languages.el ends here
