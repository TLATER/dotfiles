;; ------------------------------------------------------------
;; coding-standards.el
;;
;; Code maintenance helpers
;; ------------------------------------------------------------

;; Set up indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Electric pairs
(electric-pair-mode)
