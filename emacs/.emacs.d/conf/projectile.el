;;; projectile.el---Projectile configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 18 Oct 2016
;;
;;; Code:

(use-package projectile
  :init
  (setq projectile-create-missing-test-files t)
  :config
  (projectile-mode))

;;; projectile.el ends here
