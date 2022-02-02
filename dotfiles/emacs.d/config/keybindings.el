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

(defun autoformat ()
  "Autoformat the current buffer."
  (interactive)
  (pcase major-mode
    ('python-mode (progn
                     (py-isort-buffer)
                     (blacken-buffer)))
    ('nix-mode (nix-format-buffer))
    ('web-mode (prettier-js))
    ('rustic-mode (rustic-format-buffer))
    ('haskell-mode (haskell-mode-stylish-buffer))
    (_ (if (eglot-managed-p)
           (eglot-format)
         (message "No formatter for this file type")))))

(global-set-key (kbd "C-c l d") 'eldoc-doc-buffer)
(global-set-key (kbd "C-c l e") 'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c l f") 'xref-find-definitions-other-window)

(global-set-key (kbd "C-c f") 'autoformat)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)

(global-set-key (kbd "C-x c") 'compile)

(global-set-key (kbd "C-<backspace>") 'backward-kill-word)

(global-set-key (kbd "C-x O") 'other-window-backwards)

(global-unset-key (kbd "<XF86Back>"))
(global-unset-key (kbd "<XF86Forward>"))

(when (daemonp)
  (global-set-key (kbd "C-x C-c") 'delete-frame))

(provide 'keybindings)
;;; keybindings.el ends here
