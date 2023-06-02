;;; editing.el --- Configuration for packages and features to help with editing      -*- lexical-binding: t; -*-

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
  (defvar share-dir))

;; ----------------------------------------------------------------------------------
;; Indentation
;; ----------------------------------------------------------------------------------

;; The *best* indentation format
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; ----------------------------------------------------------------------------------
;;; Read project .editorconfig files for when the above doesn't apply
;; ----------------------------------------------------------------------------------

(use-package editorconfig
  :hook prog-mode)

;; ----------------------------------------------------------------------------------
;;; Clean up whitespace automatically
;; ----------------------------------------------------------------------------------

(use-package whitespace-cleanup-mode
  :demand
  :commands global-whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

;; ----------------------------------------------------------------------------------
;;; Add newlines to files automatically
;; ----------------------------------------------------------------------------------

(use-package files
  :ensure nil
  :functions file-name-sans-extension
  :custom
  (require-final-newline "visit-save"))

;; ----------------------------------------------------------------------------------
;;; Snippets
;; ----------------------------------------------------------------------------------

(use-package yasnippet
  :demand
  :commands (yas-global-mode yas-expand-snippet)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" share-dir)))
  :config
  (yas-global-mode 1))

(defun autoinsert-yas-expand ()
  "Expand yasnippet in current buffer."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; ----------------------------------------------------------------------------------
;;; Autoinserts
;; ----------------------------------------------------------------------------------

(use-package autoinsert
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
  (define-auto-insert
    (rx "."
        (or "C"                         ;; .C
            (and "c"                    ;; .c
                 (optional (or "c"      ;; .cc
                               "pp"     ;; .cpp
                               "xx"     ;; .cxx
                               "++")))) ;; .c++
        eos)
    ["c-file" autoinsert-yas-expand])
  (define-auto-insert (rx ".dir-locals.el") ["dir-locals" autoinsert-yas-expand])

  ;; Various helpers for the templates
  (require 'project)
  (declare-function project-root "project.el")
  (defun get-c-guard-name ()
    "Get the name for the current file as a C header guard.

     Compliant with Google's C++ style guide."
    (let* ((filename-no-ext (file-name-sans-extension (buffer-file-name)))
           (filename (or (ignore-errors (file-relative-name
                                         filename-no-ext
                                         (project-root (project-current))))
                         (file-name-nondirectory filename-no-ext))))
      (concat (replace-regexp-in-string "[^A-Z0-9]" "_" (upcase filename)) "_H_")))

  (defun get-c-header-for-file ()
    "Get the name of the C header that is associated with this file."
    (let ((stem (file-name-sans-extension (buffer-file-name)))
          (ret nil))
      (dolist
          (ext '("H" "h" "hh" "hpp" "hxx" "h++") ret)
        (when (file-exists-p (concat stem "." ext))
          (setq ret (file-name-nondirectory (concat stem "." ext)))))))

  (auto-insert-mode t))

;; ----------------------------------------------------------------------------------
;; Spell checking
;; ----------------------------------------------------------------------------------

;; Spell checking
(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-." . nil)
              ("C-," . nil)
              ("C-c $" . nil))
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))
(use-package ispell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-dictionary "en_US"))

;; ----------------------------------------------------------------------------------
;; Better paren handling
;; ----------------------------------------------------------------------------------

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
  :preface
  (declare-function smartparens-global-mode "smartparens.el")
  (declare-function sp-use-smartparens-bindings "smartparens.el")
  :config
  (smartparens-global-mode t)
  (sp-use-smartparens-bindings))

;; ----------------------------------------------------------------------------------
;; Editing comments in a separate buffer
;; ----------------------------------------------------------------------------------

(use-package separedit
  :commands #'separedit
  :bind (:map prog-mode-map
              ("C-c ;" . #'separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment t)
  (separedit-continue-fill-column t)
  (separedit-preserve-string-indentation t))

;; ----------------------------------------------------------------------------------
;; Nicer undo UI
;; ----------------------------------------------------------------------------------

(use-package vundo
  :bind
  ("C-x u" . vundo)
  (:map vundo-mode-map
        ("C-f" . vundo-forward)
        ("<right>" . vundo-forward)
        ("C-b" . vundo-backward)
        ("<left>" . vundo-backward)
        ("C-n" . vundo-next)
        ("<down>" . vundo-next)
        ("C-p" . vundo-previous)
        ("<up>" . vundo-previous)
        ("C-a" . vundo-stem-root)
        ("C-e" . vundo-stem-end)
        ("C-g" . vundo-quit)
        ("q" . vundo-confirm)
        ("RET" . vundo-confirm))
  :config
  (defun vundo-set-symbols-for-frame ()
    "Set symbols based on whether we are in a graphical display or not."
    (setq vundo-glyph-alist
          (if (display-graphic-p)
              vundo-unicode-symbols
            vundo-ascii-symbols)))
  (setq vundo-pre-enter-hook '(vundo-set-symbols-for-frame)))

(use-package undo-fu-session
  :demand
  :custom
  (undo-fu-session-directory (expand-file-name "undo-fu-session" back-dir))
  (undo-fu-session-compression 'xz)
  (undo-fu-session-file-limit 20)
  :preface
  (declare-function undo-fu-session-global-mode "undo-fu-session.el")
  :config
  (undo-fu-session-global-mode 1))

;;; editing.el ends here
