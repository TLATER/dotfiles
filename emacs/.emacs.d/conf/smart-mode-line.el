;; ------------------------------------------------------------
;; smart-mode-line.el
;;
;; Smart mode line configuration
;; ------------------------------------------------------------

;; Set up sml
(use-package smart-mode-line
  :init
  (setq sml/theme 'respectful)
  (setq custom-safe-themes
        (quote
         ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223")))
  :config
  (sml/setup))
