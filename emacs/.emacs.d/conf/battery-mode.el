;; ------------------------------------------------------------
;; battery-mode.el
;;
;; Battery mode configuration
;; ------------------------------------------------------------

(use-package battery
  :init
  (setq battery-status-function (quote battery-linux-sysfs))
  :config
  (display-battery-mode t)
  (setq battery-mode-line-format " %b%p%%"))
