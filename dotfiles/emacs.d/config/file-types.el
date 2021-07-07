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

(use-package elm-mode
  :mode ("\\.elm\\'"))

(use-package lua-mode
  :mode ("\\.lua\\'"))

(use-package pug-mode
  :mode ("\\.pug\\'"))

(use-package markdown-mode
  :mode ("\\.mdwn\\'" "/tmp/neomutt-.*")
  :init
  (setq markdown-command '("nix-shell" "-p" "pandoc" "--run" "pandoc --from=markdown --to=html5")))

(use-package gnuplot
  :mode ("\\.p\\'" "\\.gp\\'" "\\.gnuplot\\'")
  :init
  (setq gnuplot-program "gnuplot"))

(use-package yaml-mode
  :mode ("\\.bst\\'" "\\project.conf\\'"))

(use-package systemd
  :mode ("\\.service\\'" . systemd-mode))

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (require 'smartparens-rust))

(use-package cython-mode
  :mode "\\.pyx\\'")

(use-package dockerfile-mode
  :mode "\\`Dockerfile\\'")

(use-package scss-mode
  :mode ("\\.sass\\'" "\\.scss\\'"))

(use-package json-mode
  :mode ("\\.json\\'"))

(use-package jsonnet-mode
  :mode ("\\.jsonnet\\'"))

(use-package graphql-mode
  :mode ("\\.graphql\\'" "\\.gql\\'"))

(use-package protobuf-mode
  :mode ("\\.proto\\'"))

(use-package csv-mode
  :mode ("\\.csv\\'"))

(use-package nix-mode
  :mode ("\\.nix\\'")
  :bind (:map nix-mode-map
              ("C-c f" . nix-format-buffer)))

(use-package stumpwm-mode)

(use-package ahk-mode
  :mode ("\\.ahk\\'"))

;; web stuff

;; Required because we need c-populate-syntax-table
(use-package cc-mode
  :mode "\\.cc\\'")

;; Mainly use js2-mode
(use-package js2-mode
  :after (cc-mode prettier-js)
  :mode "\\.js\\'"
  :interpreter "node"
  :bind
  (:map js2-mode-map
        ("C-c f" . prettier-js))
  :init
  (setq js2-basic-offset 4)
  (setq-default js2-additional-externs
                '("$" "define" "require")))

(use-package js2-refactor
  :after (js2-mode)
  :hook js2-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package web-mode
  :after prettier-js
  :bind
  (:map web-mode-map
        ("C-c f" . prettier-js))
  :mode "\\.hbs\\'" "\\.tsx?\\'")

(use-package prettier-js)

(provide 'file-types)
;;; file-types.el ends here
