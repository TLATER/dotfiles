;;; purpose.el --- Purpose mode configuration        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniël Maat

;; Author: Tristan Daniël Maat <tm@tlater.net>
;; Keywords: local

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

(prelude-require-package 'window-purpose)
(prelude-require-package 'helm-purpose)

(purpose-mode)

;;; Side windows
;; regex
(add-to-list 'purpose-user-regexp-purposes
             '("\\*eshell\\*\\(<[[:digit:]]+>\\)?" . side-window))
(add-to-list 'purpose-user-regexp-purposes
             '("\\*pytest-?.*\\*" . side-window))
(add-to-list 'purpose-user-regexp-purposes
             '("\\*terminal\\*" . side-window))
;; name
(add-to-list 'purpose-user-name-purposes
             '("*compilation*" . side-window))
(add-to-list 'purpose-user-name-purposes
             '("*RE-Builder*" . side-window))
(add-to-list 'purpose-user-name-purposes
             '("*eww*" . side-window))

(purpose-compile-user-configuration)
(helm-purpose-setup)

(provide 'purpose)
;;; purpose.el ends here
