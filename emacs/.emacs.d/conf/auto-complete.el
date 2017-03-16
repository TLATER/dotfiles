;; ------------------------------------------------------------
;; auto-complete.el
;;
;; Auto complete mode configuration
;; ------------------------------------------------------------

;; Set up autocompletion
(use-package auto-complete
  :config
  (setq-default ac-sources (push 'ac-source-yasnippet ac-sources))
  (ac-config-default))
