;;; file-types.el --- Configuration for modes to handle different file types  -*- lexical-binding: t; -*-

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

(defvar share-dir)
(eval-and-compile
  (require 'use-package)
  (require 'eglot-config))

(use-package haskell-mode
  :commands haskell-mode-stylish-buffer
  :mode (rx ".hs" string-end)
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c `" . haskell-interactive-bring)))

(use-package groovy-mode
  :mode (rx ".groovy" string-end))

(use-package bazel
  :mode (((rx ".bzl" string-end) . bazel-starlark-mode)
         ((rx (or "BUILD" "BUILD.bazel") string-end) . bazel-build-mode))
  :commands bazel-buildifier)

(use-package kotlin-mode
  :mode (rx ".kt" string-end))

(use-package elm-mode
  :mode (rx ".elm" string-end))

(use-package lua-mode
  :mode (rx ".lua" string-end))

(use-package markdown-mode
  :mode (rx (or
             (and (or ".md" ".mdwn") string-end)
             (and string-start "/tmp/neomutt-")))
  :init
  (setq markdown-command '("nix-shell"
                           "-p" "pandoc"
                           "--run" "pandoc --from=markdown --to=html5")))

(use-package gnuplot
  :mode ((rx (or ".p" ".gp" ".gnuplot") string-end) . gnuplot-mode)
  :init
  (setq gnuplot-program "gnuplot"))

(use-package yaml-mode
  :mode (rx (or ".yaml" ".yml" ".bst"
                (and string-start "project.conf")) string-end))

(use-package systemd
  :mode ((rx (or".service" ".socket" ".device" ".mount" ".automount"
                ".swap" ".target" ".path" ".timer" ".slice" ".scope")
             string-end)
         . systemd-mode))

(use-package rust-mode
  :mode ((rx ".rs" string-end) . rust-mode))

(use-package cargo-mode
  :mode ((rx (or (and ".rs" string-end)
                 (and string-start "Cargo.toml" string-end)))
         . rust-mode))

(use-package cython-mode
  :mode (rx ".pyx" string-end))

(use-package dockerfile-mode
  :mode (rx string-start "Dockerfile" string-end))

(use-package scss-mode
  :mode (rx (or ".sass" ".scss") string-end))

(use-package json-mode
  :mode (rx ".json" string-end))

(use-package jsonnet-mode
  :mode (rx ".jsonnet" string-end))

(use-package graphql-mode
  :mode (rx (or ".graphql" ".gql") string-end))

(use-package protobuf-mode
  :mode (rx ".proto" string-end))

(use-package csv-mode
  :mode (rx ".csv" string-end))

(use-package nix-mode
  :commands nix-format-buffer
  :mode (rx ".nix" string-end))

(use-package stumpwm-mode
  :mode (rx "stumpwm/" (* anychar) string-end))

(use-package ahk-mode
  :mode (rx ".ahk" string-end))

;; web stuff

(use-package cc-mode
  :ensure nil
  :functions c-populate-syntax-table)

(use-package web-mode
  :bind
  :mode (rx (or ".pug" ".hbs" (and ".ts" (? "x"))) string-end))

(use-package prettier-js
  :commands prettier-js)

;; Auto-insert settings
(use-package yasnippet
  :demand
  :commands (yas-global-mode yas-expand-snippet)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" share-dir)))
  :config
  (make-directory (expand-file-name "snippets" share-dir) t)
  (yas-global-mode 1))

(defun autoinsert-yas-expand ()
  "Expand yasnippet in current buffer."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(use-package autoinsert
  :demand
  :ensure nil
  :custom
  ;; Remove all the built-in auto-inserts, so we can replace them with yatemplate
  ;; templates instead (more convenient to fill).
  (auto-insert-alist nil)
  (auto-insert-query nil)
  (auto-insert-directory (expand-file-name "templates/" share-dir))

  :config
  (define-auto-insert 'emacs-lisp-mode ["emacs-lisp-mode" autoinsert-yas-expand])
  (define-auto-insert 'html-mode ["html" auto-insert-yas-expand])
  (define-auto-insert (rx "flake.nix") ["flake.nix" autoinsert-yas-expand])
  (define-auto-insert (rx "shell.nix") ["shell.nix" autoinsert-yas-expand])
  (define-auto-insert
    (rx bos "standup-" (one-or-more anything) ".mdwn" eos)
    ["standup-notes" autoinsert-yas-expand])
  (define-auto-insert
    (rx bos "week-" (one-or-more anything) ".org" eos)
    ["week-agenda" autoinsert-yas-expand])
  (define-auto-insert
    (rx "."
        (or "H"                         ;; .H
            (and "h"                    ;; .h
                 (optional (or "h"      ;; .hh
                               "pp"     ;; .hpp
                               "xx"     ;; .hxx
                               "++")))) ;; .h++
        eos)
    ["c-header" autoinsert-yas-expand])

  (auto-insert-mode t))

(use-package files
  :ensure nil
  :defer t)
(use-package project
  :ensure nil
  :defer t
  :functions project-root)

;; Template helpers
(defun get-c-guard-name ()
  "Get the name for the current file as a C header guard.

   Compliant with Google's C++ style guide."
  (let* ((filename-no-ext (file-name-sans-extension (buffer-file-name)))
         (filename (or (ignore-errors (file-relative-name
                                       filename-no-ext
                                       (project-root (project-current))))
                       (file-name-nondirectory filename-no-ext))))
    (concat (replace-regexp-in-string "[^A-Z0-9]" "_" (upcase filename)) "_H_")))

;; Autoformatting settings

(use-package reformatter
  :commands (alejandra-format-region alejandra-format-buffer)
  :functions reformatter--do-region
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
      :lighter " AL")))

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
    ('web-mode (prettier-js))
    ('haskell-mode (haskell-mode-stylish-buffer))
    ((or 'bazel-mode
         (app (lambda (m) (get m 'derived-mode-parent)) 'bazel-mode))
     (bazel-buildifier))
    (_ (if (eglot-managed-p)
           (eglot-format)
         (message "No formatter for this file type")))))

(global-set-key (kbd "C-c f") 'autoformat)

(provide 'file-types)
;;; file-types.el ends here
