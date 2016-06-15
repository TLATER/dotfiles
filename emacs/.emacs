(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Add MELPA package repository
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(setq package-load-list '((ein nil) (ein-mumamo nil) all))
(package-initialize)

;; Allow use-package to install packages if not present
(setq use-package-always-ensure t)

;; Load custom functions
(load-file "~/.emacs.d/conf/unindent-region.el")
(load-file "~/.emacs.d/conf/eval-and-replace.el")

;; Load the sub-configurations
(load-file "~/.emacs.d/conf/auto-complete.el")
(load-file "~/.emacs.d/conf/auto-inserts.el")
(load-file "~/.emacs.d/conf/coding-standards.el")
(load-file "~/.emacs.d/conf/ecb.el")
(load-file "~/.emacs.d/conf/keybindings.el")
(load-file "~/.emacs.d/conf/linum-mode.el")
(load-file "~/.emacs.d/conf/magit.el")
(load-file "~/.emacs.d/conf/misc.el")
(load-file "~/.emacs.d/conf/modeline-info.el")
(load-file "~/.emacs.d/conf/org.el")
(load-file "~/.emacs.d/conf/screensaver.el")
(load-file "~/.emacs.d/conf/smart-mode-line.el")
(load-file "~/.emacs.d/conf/text-aid-too-mode.el")
(load-file "~/.emacs.d/conf/themes.el")
