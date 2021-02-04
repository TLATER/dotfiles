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
   ("C-c r" . lsp-rename)
   ("C-c f" . lsp-format-buffer)
   ("C-c h" . lsp-describe-thing-at-point))
  :init
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

  :config
  ;; Enable .dir-locals config loading
  (add-hook 'hack-local-variables-hook
            (lambda () (when (derived-mode-p 'XXX-mode) (lsp)))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after (lsp-mode))

(provide 'lsp)
;;; lsp.el ends here
