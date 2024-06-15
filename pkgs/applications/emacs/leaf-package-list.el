;;; leaf-package-list.el --- List packages required by leaf      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Tristan Daniël Maat

;; Author: Tristan Daniël Maat <tm@tlater.net>
;; Keywords:

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

(require 'json)
(require 'package)

(defun leaf-package-list (script)
  "List packages configured with leaf in SCRIPT."

  (defvar leaf-package-list-running-p t)
  (defvar leaf-package-list--packages nil)

  (defmacro leaf (package &rest args)
    (when (and (not (package-built-in-p package))
               (if (plist-member args :ensure)
                   (not (eq (plist-get args :ensure) nil))
                 t))
      `(add-to-list 'leaf-package-list--packages (symbol-name (quote ,package)))))
  (load script nil nil t)
  (princ (if leaf-package-list--packages
             (json-encode leaf-package-list--packages)
           "[]"))
  leaf-package-list--packages)

(provide 'leaf)
;;; leaf-package-list.el ends here
