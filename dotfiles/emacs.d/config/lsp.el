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
  :hook ((rust-mode python-mode sh-mode) . lsp)
  :bind
  ("C-c r" . lsp-rename)
  :init
  ;; We want flycheck, not flymake
  (setq lsp-prefer-flymake nil)
  (setq lsp-pyls-plugins-pydocstyle-enabled t)
  :config
  ;; Add pylint setting to pyls - this isn't currently included for
  ;; some reason
  (defcustom lsp-pyls-plugins-pylint-enabled t
    "Enable or disable the plugin."
    :type 'boolean
    :group 'pyls-ls)
  (lsp-register-custom-settings '(("pyls.plugins.pylint-enabled" lsp-pyls-plugins-pylint-enabled))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :after (lsp-mode))

(use-package company-lsp
  :after (lsp-mode company)
  :init
  (setq company-lsp-cache-candidates 'auto)
  :config
  (add-to-list 'company-backends 'company-lsp))

(provide 'lsp)
;;; lsp.el ends here
