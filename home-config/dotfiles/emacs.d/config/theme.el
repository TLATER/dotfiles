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
  :defines gotham-tty-16-colors
  :custom
  (gotham-tty-256-colors t)
  (gotham-tty-16-colors t)
  :config
  (defun gotham-pick-color (color-256 color-16 color-8)
    (if gotham-tty-256-colors color-256
      (if gotham-tty-16-colors color-16 color-8)))

  (setq gotham-color-alist
        `((base0   "#0f0f0f" ,(gotham-pick-color "color-232" "black"        "black"))
          (base1   "#11151c" ,(gotham-pick-color "color-233" "brightblack"  "black"))
          (base2   "#091f2e" ,(gotham-pick-color "color-17"  "brightgreen"  "black"))
          (base3   "#0a3749" ,(gotham-pick-color "color-18"  "brightblue"   "blue"))
          (base4   "#245361" ,(gotham-pick-color "color-24"  "brightyellow" "cyan"))
          (base5   "#268bd2" ,(gotham-pick-color "color-81"  "brightcyan"   "cyan"))
          (base6   "#99d1ce" ,(gotham-pick-color "color-122" "white"        "white"))
          (base7   "#d3ebe9" ,(gotham-pick-color "color-194" "brightwhite"  "white"))

          (red     "#dc322f" ,(gotham-pick-color "color-124" "red"           "red"))
          (orange  "#d26937" ,(gotham-pick-color "color-166" "brightred"     "yellow"))
          (yellow  "#b58900" ,(gotham-pick-color "color-214" "yellow"        "yellow"))
          (magenta "#707880" ,(gotham-pick-color "color-67"  "brightmagenta" "white"))
          (violet  "#4e5166" ,(gotham-pick-color "color-60"  "magenta"       "blue"))
          (blue    "#195466" ,(gotham-pick-color "color-24"  "blue"          "blue"))
          (cyan    "#599cab" ,(gotham-pick-color "color-44"  "cyan"          "cyan"))
          (green   "#2aa889" ,(gotham-pick-color "color-78"  "green"         "green"))))
  (load-theme 'gotham t))

;;; theme.el ends here
