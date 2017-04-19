;;; minors.el --- Minor mode configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat <mbax4tm2@kilburn.cs.man.ac.uk>
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

;; YASnippet
(define-key yas-minor-mode-map (kbd "C-c <tab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "C-c TAB") 'yas-expand)
(yas-global-mode 1)

;; Magit
(global-set-key (kbd "C-c g s") 'magit-status)

(provide 'minors)
;;; minors.el ends here
