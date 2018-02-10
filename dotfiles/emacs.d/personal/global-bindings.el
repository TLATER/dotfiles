;;; global-bindings.el --- Global keybindings -*- lexical-binding: t; -*-

;;   Copyright (C) 2017  Tristan Daniel Maat

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

;; Paragraph movement
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Commenting
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)

;; Compiling
(global-set-key (kbd "C-x c") 'compile)

;; Deleting backwards
(defun backward-kill-line ()
  "Kill the line backwards from point"
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))
(global-set-key (kbd "M-<backspace>") 'backward-kill-line)
(global-set-key (kbd "C-<backspace>") 'backward-kill-word)

(provide 'global-bindings)
;;; global-bindings.el ends here
