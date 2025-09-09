;;; init.el --- Emacs configuration entry point      -*- lexical-binding: t; -*-

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

;; Sets up some basic variables (directory paths etc.), as well as packages and features
;; that need to be set at the very start of initialization.  After that, load the rest
;; of the configuration files.

;;; Code:

;; ----------------------------------------------------------------------------------
;;; Basic path setup
;; ----------------------------------------------------------------------------------

(defvar back-dir (expand-file-name
                  "emacs/backups"
                  (or (getenv "XDG_DATA_HOME") "~/.local/share/")))
(defvar share-dir (expand-file-name
                   "emacs/share"
                   (or (getenv "XDG_CONFIG_HOME") "~/.config")))
(defvar cache-dir (expand-file-name
                   "emacs"
                   (or (getenv "XDG_CACHE_HOME") "~/.cache")))

;; Load `custom.el'' for any imperative settings; This sets safe
;; themes and variables, which is important to get out of the way
;; early.
(setq custom-file (expand-file-name "custom.el" data-dir))
(if (file-exists-p custom-file)
    (load custom-file)
  (custom-set-variables))

;; ----------------------------------------------------------------------------------
;;; Package management
;; ----------------------------------------------------------------------------------

(require 'package)
(setq package-user-dir (expand-file-name "elpa" data-dir))
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

(require 'leaf)

;; Make sure emacs doesn't try to randomly fetch packages itself; nix is in charge of
;; that
(setq package-enable-at-startup nil)

;; ----------------------------------------------------------------------------------
;;; Set up improved garbage collection for better startup and runtime performance.
;; ----------------------------------------------------------------------------------

(leaf gcmh
  :ensure t
  :global-minor-mode t)

;; ----------------------------------------------------------------------------------
;;; Set up nix-provided theme
;; ----------------------------------------------------------------------------------

(leaf base16-theme
  :require t
  :ensure t
  :pre-setq (base16-theme-256-color-source . 'base16-shell)
  :config
  (deftheme base16-custom)
  (let ((colors '(; Background tones
                  :base00 "#002d38"
                  :base01 "#093946"

                  ; Content tones
                  ;; TODO(tlater): Fix these colors; they're not completely suited
                  ;; ATM because solarized considers these "content colors", while
                  ;; base16 only considers base03 a content color
                  :base02 "#5b7279"
                  :base03 "#657377"
                  :base04 "#98a8a8"
                  :base05 "#8faaab"

                  ; Foreground tones
                  :base06 "#f1e9d2"
                  :base07 "#fbf7ef"

                  ; Accent colors
                  :base08 "#f23749"
                  :base09 "#d56500"
                  :base0A "#ac8300"
                  :base0B "#819500"
                  :base0C "#259d94"
                  :base0D "#2b90d8"
                  :base0E "#7d80d1"
                  :base0F "#dd459d")))

    (base16-theme-define 'base16-custom colors)

    ;; Tweak the theme a little
    (base16-theme-set-faces
     'user
     colors
     '(
       ;; Spec says to make variables red, but this looks horrible
       (font-lock-variable-name-face :foreground base0D)
       ;; I don't want a red cursor either
       (cursor :background base05)

       ;; No built-in settings for these packages in the base16 theme
       (cov-none-face :foreground base08)
       (cov-heavy-face :foreground base0B)))

    (enable-theme 'base16-custom)))

;; ----------------------------------------------------------------------------------
;;; Load other configuration files
;; ----------------------------------------------------------------------------------

;; Load everything in the config directory
(let ((config-dir (expand-file-name "config" (file-name-directory load-file-name))))
  (when (file-exists-p config-dir)
    (mapc (lambda (file)
            (load (file-name-sans-extension file) nil nil nil t))
          (directory-files config-dir t "^[^#\.].*\\.el$"))))

;;; init.el ends here
