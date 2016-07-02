;;; flycheck.el---Flycheck configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 29 Jun 2016
;;
;;; Code:

(use-package flycheck
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :init
  (global-flycheck-mode))

;;; flycheck.el ends here
