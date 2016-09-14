;;; transparency.el---Transparency configuration (background image :))
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 05 Sep 2016
;;
;;; Code:

(defvar exwm-background-transparency 90
  "The desktop background transparency when no X window is shown.")

(defun exwm-toggle-frame-transparency ()
  "Toggle the current frame's transparency."
  (interactive)
  (if (equal '(90 0) (frame-parameter (selected-frame) 'alpha))
      (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (set-frame-parameter (selected-frame) 'alpha `(,exwm-background-transparency 0))))

(message "%s" (equal '(90 0) (frame-parameter (selected-frame) 'alpha)))

(setq frame-alpha-lower-limit 0)
(set-frame-parameter (selected-frame) 'alpha `(,exwm-background-transparency 0))
(add-to-list 'default-frame-alist `(alpha ,exwm-background-transparency 0))

;; (add-hook 'exwm-manage-finish-hook
;;           (lambda ()
;;             (set-frame-parameter (selected-frame)
;;                                  'alpha '(100 100))))

;; (add-hook 'exwm-workspace-switch-hook
;;           (lambda ()
;;             (if (string= major-mode "exwm-mode")
;;                 (set-frame-parameter (selected-frame)
;;                                      'alpha '(100 100)))))

;;; transparency.el ends here
