;;; python.el --- Python configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat <mbax4tm2@kilburn.cs.man.ac.uk>
;; Keywords: local, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(setq python-shell-interpreter "python3")

;; jedi
(prelude-require-package 'jedi-core)
(when (boundp 'company-backends)
  (prelude-require-package 'company-jedi)
  (add-to-list 'company-backends 'company-jedi))
;; config
(setq jedi:complete-on-dot t)

;; pytest
(prelude-require-package 'pytest)
(eval-after-load "python-mode"
  '(define-key python-mode-map (kbd "C-c t .") 'pytest-one))
(eval-after-load "python-mode"
  '(define-key python-mode-map (kbd "C-c t a") 'pytest-all))
(eval-after-load "python-mode"
  '(define-key python-mode-map (kbd "C-c t m") 'pytest-module))

(provide 'python)
;;; python.el ends here
