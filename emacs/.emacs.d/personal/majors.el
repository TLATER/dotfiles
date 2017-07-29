;;; majors.el --- Major mode configuration           -*- lexical-binding: t; -*-

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

;; Buildstream
(add-to-list 'auto-mode-alist '("\\.bst\\'" . yaml-mode))

;; Systemd
(prelude-require-package 'systemd)

;; Web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(provide 'majors)
;;; majors.el ends here
