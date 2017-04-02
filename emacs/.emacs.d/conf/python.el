;;; python.el---Python-specific configuration
;;
;; Copyright (C) 2017 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 12 Mar 2017
;;
;;; Code:

(use-package pycoverage
  :config
  (add-hook 'python-mode-hook 'pycoverage-mode))

(use-package jedi
  :init
  (setq jedi:complete-on-dot t)
  :config
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package pytest
  :bind (:map python-mode-map
              ("C-c t ." . pytest-one)
              ("C-c t a" . pythest-all)
              ("C-c t m" . pytest-module)))

;;; python.el ends here
