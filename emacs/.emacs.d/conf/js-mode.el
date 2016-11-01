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
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (define-key js2-mode-map (kbd "\C-c \C-r j") 'js-doc-insert-function-doc-snippet)
  (define-key js2-mode-map (kbd "\C-c \C-r f") 'js-doc-insert-file-doc)
  (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag))

(use-package js2-refactor
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package js-doc
  :init
  (setq js-doc-mail-address user-mail-address
        js-doc-author (format "%s <%s>" user-full-name "tm@tlater.net")
        js-doc-url "tlater.net"
        js-doc-license "UNLICENSED"))
;;; js-mode.el ends here
