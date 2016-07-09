;; ------------------------------------------------------------
;; whitespace.el
;;
;; Whitespace mode configuration
;; ------------------------------------------------------------

;; Set up global whitespace marking
(use-package whitespace
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))

  :config
  (setq whitespace-global-modes '(not text-aid-too-mode))
  (global-whitespace-mode t))
