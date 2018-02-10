;;; load-wm.el --- Add --wm switch                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat
;; Keywords: local, unix, abbrev

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

(setq load-exwm-switch (member "--wm" command-line-args))
(setq command-line-args (delete "--wm" command-line-args))

(if load-exwm-switch
    (load-file "~/.emacs.d/personal/wm/wm.el"))

(provide 'load-wm)
;;; load-wm.el ends here
