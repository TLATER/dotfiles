;;; use-package.el --- use-package configuration     -*- lexical-binding: t; -*-

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

;; We set up use-package and various utilities to make it work as we
;; like.  Done separately since it's a lot of work.

;;; Code:

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-expand-minimally byte-compile-current-file))

(require 'use-package)
(setq use-package-compute-statistics t)
(setq use-package-always-ensure t)

;; Ensure that our exec path is set up correctly
(use-package exec-path-from-shell
  :functions (exec-path-from-shell-initialize)
  :if (not (memq system-type '(cygwin windows-nt)))
  :config
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :after exec-path-from-shell)

(provide 'use-package)
;;; use-package.el ends here
