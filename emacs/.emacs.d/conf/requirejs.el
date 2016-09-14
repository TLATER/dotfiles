;;; requirejs.el---Requirejs configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 04 Sep 2016
;;
;;; Code:

(defun set-requirejs-base-to-current ()
    "Set the requirejs include base directory"
    (interactive)
    (setq requirejs-require-base (file-name-directory buffer-file-name)))

(use-package js2-mode
  :init
  (setq js2-basic-offset 4)
  (setq-default js2-additional-externs
                '("$"
                  "define"
                  "require"))

  :config
  (add-to-list 'js2-imenu-extension-styles
               '((:framework requirejs
                  :call-re "^\\s-*define\\s-*("
                  :recorder js2-imenu-record-module-pattern)))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package requirejs
  :bind
  ("C-c r b" . set-requirejs-base-to-current))

;;; requirejs.el ends here
