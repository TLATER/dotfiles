;;; keybindings.el --- Global keybindings            -*- lexical-binding: t; -*-

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

(defun backward-kill-line ()
  "Kill the line backwards from point."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun other-window-backwards ()
  "Go to the other window in reverse order."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)

(global-set-key (kbd "C-x c") 'compile)

(global-set-key (kbd "C-<backspace>") 'backward-kill-word)

(global-set-key (kbd "C-x O") 'other-window-backwards)

(global-unset-key (kbd "<XF86Back>"))
(global-unset-key (kbd "<XF86Forward>"))

(provide 'keybindings)
;;; keybindings.el ends here
