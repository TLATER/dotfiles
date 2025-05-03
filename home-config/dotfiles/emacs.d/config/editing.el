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
  (require 'leaf))

;; ----------------------------------------------------------------------------------
;; Indentation
;; ----------------------------------------------------------------------------------

;; The *best* indentation format
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; ----------------------------------------------------------------------------------
;;; Read project .editorconfig files for when the above doesn't apply
;; ----------------------------------------------------------------------------------

(leaf editorconfig
  :ensure t
  :hook (prog-mode-hook . editorconfig-mode))

;; ----------------------------------------------------------------------------------
;;; Clean up whitespace automatically
;; ----------------------------------------------------------------------------------

(leaf whitespace-cleanup-mode
  :ensure t
  :commands global-whitespace-cleanup-mode
  :global-minor-mode global-whitespace-cleanup-mode)

;; ----------------------------------------------------------------------------------
;;; Add newlines to files automatically
;; ----------------------------------------------------------------------------------

(leaf files
  :custom (require-final-newline . "visit-save")
  :custom (major-mode-remap-alist . '((sh-mode . bash-ts-mode)
                                      (css-mode . css-ts-mode))))

;; ----------------------------------------------------------------------------------
;;; Snippets
;; ----------------------------------------------------------------------------------

(leaf yasnippet
  :ensure t
  :custom
  `(yas-snippet-dirs . '(,(expand-file-name "snippets" share-dir)))
  :global-minor-mode yas-global-mode
  :defun yas-expand-snippet
  :config
  (defun autoinsert-yas-expand ()
    "Expand yasnippet in current buffer."
    (yas-expand-snippet (buffer-string) (point-min) (point-max))))

;; ----------------------------------------------------------------------------------
;;; Autoinserts
;; ----------------------------------------------------------------------------------

(leaf autoinsert
  :after yasnippet
  :custom
  ;; Remove all the built-in auto-inserts, so we can replace them with yatemplate
  ;; templates instead (more convenient to fill).
  (auto-insert-alist . nil)
  (auto-insert-query . nil)
  `(auto-insert-directory . ,(expand-file-name "templates/" share-dir))
  :global-minor-mode auto-insert-mode
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
                                         (if (project-current nil)
                                             (project-root (project-current)))))
                         (file-name-nondirectory filename-no-ext))))
      (concat (replace-regexp-in-string "[^A-Z0-9]" "_" (upcase filename)) "_H_")))

  (defun get-c-header-for-file ()
    "Get the name of the C header that is associated with this file."
    (let ((stem (file-name-sans-extension (buffer-file-name)))
          (ret nil))
      (dolist
          (ext '("H" "h" "hh" "hpp" "hxx" "h++") ret)
        (when (file-exists-p (concat stem "." ext))
          (setq ret (file-name-nondirectory (concat stem "." ext))))))))

;; ----------------------------------------------------------------------------------
;; Spell checking
;; ----------------------------------------------------------------------------------

;; Spell checking
(leaf flyspell
  :bind (:flyspell-mode-map
         ("C-;" . nil)
         ("C-." . nil)
         ("C-," . nil)
         ("C-c $" . nil))
  :hook ((prog-mode-hook . flyspell-prog-mode)
         (text-mode-hook . flyspell-mode)))
(leaf ispell
  :custom
  (ispell-program-name . "aspell")
  (ispell-extra-args . '("--sug-mode=ultra"))
  (ispell-dictionary . "en_US"))

;; ----------------------------------------------------------------------------------
;; Better paren handling
;; ----------------------------------------------------------------------------------

(leaf smartparens
  :ensure t
  :defun sp-use-smartparens-bindings
  :bind (:smartparens-mode-map
         ("C-S-a" . sp-beginning-of-sexp)
         ("C-S-e" . sp-end-of-sexp)
         ("M-<left>" . sp-backward-up-sexp)
         ("M-<right>" . sp-down-sexp)
         ("M-<down>" . sp-forward-sexp)
         ("M-<up>" . sp-backward-sexp))
  :custom
  (sp-navigate-interactive-always-progress-point . t)
  :global-minor-mode smartparens-global-mode
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings))

;; ----------------------------------------------------------------------------------
;; Editing comments in a separate buffer
;; ----------------------------------------------------------------------------------

(leaf separedit
  :ensure t
  :bind (:prog-mode-map
         :package prog-mode
         ("C-c ;" . #'separedit))
  :custom
  (separedit-default-mode . 'markdown-mode)
  (separedit-remove-trailing-spaces-in-comment . t)
  (separedit-continue-fill-column . t)
  (separedit-preserve-string-indentation . t))

;; ----------------------------------------------------------------------------------
;; Nicer undo UI
;; ----------------------------------------------------------------------------------

;; The change group at which this size is exceeded is the last one
;; kept.
(setq undo-limit         (* 2 1024 1024))
;; The change group at which this size is exceeded is discarded itself
;; (along with all older change groups). There is one exception: the
;; very latest change group is only discarded if it exceeds
;; ‘undo-outer-limit’.
(setq undo-strong-limit (* 10 1024 1024))
;; If at garbage collection time the undo info for the current command
;; exceeds this limit, Emacs discards the info and displays a
;; warning. This is a last ditch limit to prevent memory overflow.
(setq undo-outer-limit (* 50 1024 1024))

(leaf vundo
  :ensure t
  :bind
  ("C-x u" . vundo)
  (:vundo-mode-map
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
  :defvar (vundo-glyph-alist vundo-unicode-symbols vundo-ascii-symbols vundo-pre-enter-hook)
  :config
  (defun vundo-set-symbols-for-frame ()
    "Set symbols based on whether we are in a graphical display or not."
    (setq vundo-glyph-alist
          (if (display-graphic-p)
              vundo-unicode-symbols
            vundo-ascii-symbols)))
  (setq vundo-pre-enter-hook '(vundo-set-symbols-for-frame)))

(leaf undo-fu-session
  :ensure t
  :custom
  `(undo-fu-session-directory . ,(expand-file-name "undo-fu-session" back-dir))
  (undo-fu-session-compression . 'xz)
  :global-minor-mode undo-fu-session-global-mode)

;;; editing.el ends here
