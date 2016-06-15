;; ------------------------------------------------------------
;; auto-complete.el
;;
;; Auto complete mode configuration
;; ------------------------------------------------------------

;; Set up autocompletion
(use-package auto-complete
  :config
  (ac-config-default))

(use-package jedi
  :init
  (setq jedi:complete-on-dot t)

  :config
  (add-hook 'python-mode-hook 'jedi:setup))
