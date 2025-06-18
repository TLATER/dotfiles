;;; color-template.el --- Color scheme template to pass to emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Tristan Daniël Maat

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

;; Simple set of colors to be passed on to Emacs.

;;; Code:

(defvar color-template-colors
  '(:base00 "@base00@"
            :base01 "@base01@"
            :base02 "@base02@"
            :base03 "@base03@"
            :base04 "@base04@"
            :base05 "@base05@"
            :base06 "@base06@"
            :base07 "@base07@"
            :base08 "@base08@"
            :base09 "@base09@"
            :base0A "@base0A@"
            :base0B "@base0B@"
            :base0C "@base0C@"
            :base0D "@base0D@"
            :base0E "@base0E@"
            :base0F "@base0F@")
  "Custom colors for a base16 scheme.")

(provide 'color-template)
;;; color-template.el ends here
