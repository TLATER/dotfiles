;;; daemon.el --- Settings for when running as a daemon      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tristan Daniël Maat

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

(when (daemonp)
  ;; Stop prompting whether we really want to stop when we have a
  ;; running process (this just clogs up `systemctl --user stop emacs`
  ;; without ever being useful, since emacs is running as a daemon
  ;; anyway).
  (setq confirm-kill-processes 'nil)

  ;; Rebind the quit command to kill the current frame instead
  (define-key global-map (kbd "C-x C-c") 'delete-frame))

;;; daemon.el ends here
