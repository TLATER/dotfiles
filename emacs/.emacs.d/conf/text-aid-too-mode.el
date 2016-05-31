;; ------------------------------------------------------------
;; text-aid-too-mode.el
;;
;; A mode for the text-aid-too chrome extension
;; ------------------------------------------------------------

(define-derived-mode text-aid-too-mode html-mode
  (setq mode-name "Text-Aid-Too"))

(add-hook 'text-aid-too-mode-hook (lambda () (flyspell-mode t)))

(add-to-list 'auto-mode-alist
             '("tlater-text-aid-too-[-[:alnum:]]*\..*" . text-aid-too-mode))
