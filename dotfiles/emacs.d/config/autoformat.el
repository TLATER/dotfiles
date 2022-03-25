;;; autoformat.el --- Auto formatting functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tristan Daniël Maat

;; Author: Tristan Daniël Maat(use-package reformatter <tm@tlater.net>
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

(use-package reformatter
  :config
  (reformatter-define alejandra-format
    :program "alejandra"
    :lighter " AL"))

(defcustom use-nixfmt nil
  "Use nixfmt for formatting nix instead of alejandra."
  :type 'boolean
  :local 'booleanp
  :group 'autoformat)

(defun autoformat ()
  "Autoformat the current buffer."
  (interactive)
  (pcase major-mode
    ('nix-mode (if use-nixfmt
                   (nix-format-buffer)
                 (alejandra-format-buffer)))
    ('web-mode (prettier-js))
    ('rustic-mode (rustic-format-buffer))
    ('haskell-mode (haskell-mode-stylish-buffer))
    ((or 'bazel-mode
         (app (lambda (m) (get m 'derived-mode-parent)) 'bazel-mode))
     (bazel-buildifier))
    (_ (if (eglot-managed-p)
           (eglot-format)
         (message "No formatter for this file type")))))

(global-set-key (kbd "C-c f") 'autoformat)

(provide 'autoformat)
;;; autoformat.el ends here
