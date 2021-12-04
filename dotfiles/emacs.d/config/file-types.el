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

(use-package haskell-mode
  :mode (rx ".hs" string-end)
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c `" . haskell-interactive-bring)))

(use-package groovy-mode
  :mode (rx ".groovy" string-end))

(use-package bazel)

(use-package kotlin-mode
  :mode (rx ".kt" string-end))

(use-package elm-mode
  :mode (rx ".elm" string-end))

(use-package lua-mode
  :mode (rx ".lua" string-end))

(use-package markdown-mode
  :mode (rx (or (and (or ".md" ".mdwn") string-end) (and string-start "/tmp/neomutt-")))
  :init
  (setq markdown-command '("nix-shell" "-p" "pandoc" "--run" "pandoc --from=markdown --to=html5")))

(use-package gnuplot
  :mode ((rx (or ".p" ".gp" ".gnuplot") string-end) . gnuplot-mode)
  :init
  (setq gnuplot-program "gnuplot"))

(use-package yaml-mode
  :mode (rx (or ".yaml" ".yml" ".bst" (and string-start "project.conf")) string-end))

(use-package systemd
  :mode ((rx (or".service" ".socket" ".device" ".mount" ".automount"
                ".swap" ".target" ".path" ".timer" ".slice" ".scope")
             string-end)
         . systemd-mode))

(use-package rustic
  :mode ((rx ".rs" string-end) . rustic-mode)
  :config
  (require 'smartparens-rust)
  (setq rustic-lsp-client 'eglot)
  (remove-hook 'rustic-mode-hook 'flycheck-mode))

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
  :mode (rx ".nix" string-end))

(use-package stumpwm-mode
  :mode (rx "stumpwm/" (* anychar) string-end))

(use-package ahk-mode
  :mode (rx ".ahk" string-end))

;; web stuff

(use-package cc-mode
  :ensure nil
  :functions (c-populate-syntax-table))

(use-package web-mode
  :bind
  :mode (rx (or ".pug" ".hbs" (and ".ts" (? "x"))) string-end))

(use-package prettier-js
  :functions (prettier-js))

(provide 'file-types)
;;; file-types.el ends here
