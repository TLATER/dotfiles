;;; init.lisp --- StumpWM configuration -*- lexical-binding: t; mode: lisp; eval: (stumpwm-mode 1) -*-

;; Copyright (C) 2017  Tristan Daniël Maat

;; Author: Tristan Daniël Maat <tm@tlater.net>
;; Keywords: local

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

(in-package :stumpwm)
(load "~/.local/lib/quicklisp/setup.lisp")

;; Disable the startup message
(setf *startup-message* nil)

;; ;; Notifications:
;; (load-module "notify")
;; (notify-server-toggle)

;;; Visual:
;; Colors
(let ((base0   "#0f0f0f")
      (base1   "#11151c")
      (base2   "#091f2e")
      (base3   "#0a3749")
      (base4   "#245361")
      (base5   "#268bd2")
      (base6   "#99d1ce")
      (base7   "#d3ebe9")

      (red     "#dc322f")
      (orange  "#d26937")
      (yellow  "#b58900")
      (magenta "#707880")
      (violet  "#4e5166")
      (blue    "#195466")
      (cyan    "#599cab")
      (green   "#2aa889"))

  (set-bg-color base0)
  (set-fg-color base5)
  (set-focus-color base5)
  (set-border-color base5)
  (set-unfocus-color base4)

  (set-win-bg-color base0)

  (setf *mode-line-background-color* base0)
  (setf *mode-line-foreground-color* base4)
  (setf *mode-line-border-color* base3))

;; Window settings
(setf *window-border-style* :tight)

;; Font
;; (ql:quickload "clx-truetype")
;; (load-module "ttf-fonts")
;; (push "/usr/local/share/fonts/" xft:*font-dirs*)
;; (set-font (make-instance 'xft:font
;;                         :family "Noto Sans Mono"
;;                         :subfamily "Regular"
;;                         :size 24))

;; Gaps:
(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 20)
(setf swm-gaps:*outer-gaps-size* 28)
(define-key *root-map* (kbd "u") "toggle-gaps")

;;; Keys:
(define-key *root-map* (kbd "c") "exec ${VTERM:-xterm}")
(define-key *root-map* (kbd "C-c") "exec ${VTERM:-xterm}")
(define-key *root-map* (kbd "e") "exec emacsclient -c -a 'emacs'")
(define-key *root-map* (kbd "C-e") "exec emacsclient -c -a 'emacs'")
(define-key *root-map* (kbd ".") "exec rofi -show drun")
(define-key *root-map* (kbd "C-.") "exec rofi -show drun")
(define-key *root-map* (kbd ",") "exec pass-rofi")
(define-key *root-map* (kbd "C-,") "exec pass-rofi")

;; Audio:
(setf up "exec pactl set-sink-volume @DEFAULT_SINK@ +5%")
(setf down "exec pactl set-sink-volume @DEFAULT_SINK@ -5%")
(setf mute "exec pactl set-sink-mute @DEFAULT_SINK@ toggle")

(define-key *top-map* (kbd "XF86AudioRaiseVolume") up)
(define-key *top-map* (kbd "XF86AudioLowerVolume") down)
(define-key *top-map* (kbd "XF86AudioMute") mute)
(define-key *root-map* (kbd ">") up)
(define-key *root-map* (kbd "<") down)

;; Brightness
(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec xbacklight +5%")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec xbacklight -5%")

;; Locking
(define-key *root-map* (kbd "L") "exec light-locker-command -l")

;; Pinentry
(ql:quickload "cffi")
(ql:quickload "usocket-server")
(ql:quickload "percent-encoding")
(load-module "pinentry")

;;; Mode line
(ql:quickload "xembed")
(load-module "stumptray")
(load-module "battery-portable")

;; Helper functions
(defun has-battery-p ()
  "Whether the current system has a battery or not"
  (if (battery-portable::all-batteries
       (battery-portable::preferred-battery-method))
      't
      'nil))

(setf stumptray:*tray-placeholder-pixels-per-space* 22)

(setf *time-modeline-string* "%k:%M")
(setf *mode-line-position* :bottom)

(setf *screen-mode-line-format*
      (list "[^B%n^b]"                   ;; Current group name
            "^>"                         ;; Skip to other side
            "%d "                        ;; Current time
            (if (has-battery-p) "| %B ") ;; Battery status
            "| %T"))                     ;; Tray

(mode-line)
(stumptray:stumptray)
(swm-gaps:toggle-gaps)

;;; Autostart
(run-shell-command "dex -ae stumpwm")

;;; init.lisp ends here

;; Local Variables:
;; stumpwm-shell-program: "/usr/bin/stumpish"
;; End:
