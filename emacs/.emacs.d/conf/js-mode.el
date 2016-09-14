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

;;; js-mode.el ends here
