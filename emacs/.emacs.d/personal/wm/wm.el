;;; wm.el --- exwm configuration                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat <mbax4tm2@E-C07KI1803.it.manchester.ac.uk>
;; Keywords: local, unix

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

;; Load exwm
(prelude-require-package 'exwm)
(require 'exwm)

;; init
(setq exwm-workspace-number 9)

;; config
(server-start)

(add-hook 'exwm-update-class-hook
          (lambda ()
            (rename-buffer exwm-class-name t)))

;; Load custom functions and keybindings
(load-file "~/.emacs.d/personal/wm/launch.el")
(load-file "~/.emacs.d/personal/wm/volume.el")
(load-file "~/.emacs.d/personal/wm/brightness.el")
(load-file "~/.emacs.d/personal/wm/keybindings.el")
(load-file "~/.emacs.d/personal/wm/transparency.el")

;; Enable the systemtray
(require 'exwm-systemtray)
(exwm-systemtray-enable)
(exwm-enable)

(provide 'wm)
;;; wm.el ends here
