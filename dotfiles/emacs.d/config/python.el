;;; python.el --- Python configuration               -*- lexical-binding: t; -*-

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

(use-package python
             :config
             (when (executable-find "ipython3")
               (setq python-shell-interpreter "ipython3"
                     python-shell-interpreter-args "--simple-prompt -i")))

(use-package jedi-core
             :init
             (setq jedi:complete-on-dot t)
             :config
             (eval-after-load "python-mode"
               '(define-key python-mode-map (kbd "C-.") 'jedi:goto-definition)))

(use-package company-jedi
             :after (company jedi-core)
             :config
             (add-to-list 'company-backends 'company-jedi))

(use-package pytest
             :bind (:map python-mode-map
                         ("C-c t ." . pytest-one)
                         ("C-c t a" . pytest-all)
                         ("C-c t m" . pytest-module)))

(provide 'python)
;;; python.el ends here
