;;; eglot.el --- Language server integration configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Tristan Daniël Maat

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

(use-package eldoc
  :ensure nil
  :defines eldoc-documentation-strategy
  :functions eldoc-documentation-compose
  :commands eldoc-doc-buffer
  :bind (("C-c l d" . 'eldoc-doc-buffer))
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p .15)
  (eldoc-echo-area-display-truncation-message 'nil)
  :config
  (global-eldoc-mode))

(defun set-eldoc-compose ()
  "Set documentation strategy to compose to work around flymake interference."
  ;; TODO: Find a nicer way to implement
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
        ("C-c l a" . eglot-code-actions))
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
  ;; Rust-analyzer is "quirky", and doesn't support updating its
  ;; configuration settings once it's running. This means that eglot
  ;; has to configure it *before* it runs, which it doesn't do by
  ;; default.
  (cl-defmethod eglot-initialization-options ((server eglot-lsp-server))
    (pcase (eglot--major-mode server)
      ('rust-mode '(:checkOnSave
                    (:command "clippy")))
      (_ eglot--{})))

  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil"))))

(provide 'eglot-config)
;;; eglot-config.el ends here
