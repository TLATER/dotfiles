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

(use-package eglot
  :functions (eglot)
  :hook ((web-mode rust-mode python-mode sh-mode c-mode c++-mode) . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio"))))

;;; eglot.el ends here
