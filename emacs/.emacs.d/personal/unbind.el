;;; unbind.el --- Unbind prelude keys                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat <mbax4tm2@E-C07KI1803.it.manchester.ac.uk>
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

(define-key prelude-mode-map (kbd "s-p") nil)
(define-key prelude-mode-map (kbd "s-r") nil)
(define-key prelude-mode-map (kbd "s-j") nil)
(define-key prelude-mode-map (kbd "s-k") nil)
(define-key prelude-mode-map (kbd "s-m") nil)
(define-key prelude-mode-map (kbd "C-c TAB") nil)
(define-key prelude-mode-map (kbd "C-c g") nil)
(define-key prelude-mode-map (kbd "C-<backspace>") nil)

(define-key flyspell-mode-map (kbd "C-;") nil)

(provide 'unbind)
;;; unbind.el ends here
