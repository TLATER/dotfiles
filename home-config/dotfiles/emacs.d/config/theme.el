;;; theme.el --- Set up my emacs theme      -*- lexical-binding: t; -*-

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

(eval-and-compile
  (require 'use-package))

(use-package gotham-theme
  :init
  (setq gotham-tty-256-colors t)

  (setq gotham-color-alist
        `((base0   "#0f0f0f" ,(if gotham-tty-256-colors "color-232" "black"))
          (base1   "#11151c" ,(if gotham-tty-256-colors "color-233" "brightblack"))
          (base2   "#091f2e" ,(if gotham-tty-256-colors "color-17"  "brightgreen"))
          (base3   "#0a3749" ,(if gotham-tty-256-colors "color-18"  "brightblue"))
          (base4   "#245361" ,(if gotham-tty-256-colors "color-24"  "brightyellow"))
          (base5   "#268bd2" ,(if gotham-tty-256-colors "color-81"  "brightcyan"))
          (base6   "#99d1ce" ,(if gotham-tty-256-colors "color-122" "white"))
          (base7   "#d3ebe9" ,(if gotham-tty-256-colors "color-194" "brightwhite"))

          (red     "#dc322f" ,(if gotham-tty-256-colors "color-124" "red"))
          (orange  "#d26937" ,(if gotham-tty-256-colors "color-166" "brightred"))
          (yellow  "#b58900" ,(if gotham-tty-256-colors "color-214" "yellow"))
          (magenta "#707880" ,(if gotham-tty-256-colors "color-67"  "brightmagenta"))
          (violet  "#4e5166" ,(if gotham-tty-256-colors "color-60"  "magenta"))
          (blue    "#195466" ,(if gotham-tty-256-colors "color-24"  "blue"))
          (cyan    "#599cab" ,(if gotham-tty-256-colors "color-44"  "cyan"))
          (green   "#2aa889" ,(if gotham-tty-256-colors "color-78"  "green"))))
  (load-theme 'gotham t))

;;; theme.el ends here
