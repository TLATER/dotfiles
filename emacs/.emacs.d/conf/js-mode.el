;;; js-mode.el---JS mode configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 08 Aug 2016
;;
;;; Code:

(add-hook 'js-mode-hook 'js-conf-hook)
(defun js-conf-hook ()
  (setq-local comment-multi-line t)
  (local-set-key [remap indent-new-comment-line] 'c-indent-new-comment-line))


(use-package js2-mode
  :init
  (setq js2-basic-offset 4)
  (setq-default js2-additional-externs
                '("$"
                  "define"
                  "require"))
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


;;; js-mode.el ends here
