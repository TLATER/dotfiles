;;; lsp.el --- Language server integration configuration  -*- lexical-binding: t; -*-

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

(use-package lsp-mode
  :functions (lsp-register-custom-settings)
  :hook ((web-mode rust-mode python-mode sh-mode c-mode c++-mode) . lsp)
  :bind
  (:map lsp-mode-map
   ("C-c l r" . lsp-rename)
   ("C-c l d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-server-install-dir (expand-file-name "lsp/servers" data-dir))
  (setq lsp-session-file (expand-file-name "lsp-session-v1" data-dir)
        lsp-ui-doc-enable nil)

  ;; We want flycheck, not flymake
  (setq lsp-prefer-flymake nil)

  ;;; Python
  (setq lsp-pyls-plugins-pydocstyle-enabled t)
  (setq lsp-pyls-plugins-pylint-args '("--rcfile=/home/tlater/.config/pylint"))
  (add-to-list 'safe-local-variable-values
               '(lsp-pyls-plugins-pylint-args "--rcfile=/home/tlater/Documents/Work/buildstream/.pylintrc" "--enable-=fixme"))

  ;;; Rust
  (setq lsp-rust-clippy-preference "on")
  (setq lsp-rust-analyzer-proc-macro-enable 't)

  ;; See https://github.com/flycheck/flycheck/issues/1762#issuecomment-749789589
  ;; Add buffer local Flycheck checkers after LSP for different major modes.
  (defvar-local lsp-flycheck-local-cache nil)
  (defun lsp-flycheck-local-checker-get (fn checker property)
    ;; Only check the buffer local cache for the LSP checker, otherwise we get
    ;; infinite loops.
    (if (eq checker 'lsp)
        (or (alist-get property lsp-flycheck-local-cache)
            (funcall fn checker property))
      (funcall fn checker property)))

  (advice-add 'flycheck-checker-get
              :around 'lsp-flycheck-local-checker-get)

  :config
  ;; Enable .dir-locals config loading
  (add-hook 'hack-local-variables-hook
            (lambda () (when (derived-mode-p 'XXX-mode) (lsp))))

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'sh-mode)
                (setq lsp-flycheck-local-cache '((next-checkers . (sh-shellcheck))))))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after (lsp-mode))

(use-package lsp-pyright)

(provide 'lsp)
;;; lsp.el ends here
