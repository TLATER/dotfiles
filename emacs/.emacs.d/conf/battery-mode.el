;; ------------------------------------------------------------
;; battery-mode.el
;;
;; Battery mode configuration
;; ------------------------------------------------------------

(use-package battery
  :init
  (setq battery-status-function (quote battery-linux-sysfs))
  :config
  (setq-default battery-mode-line-format " %b%p%% ")
  (display-battery-mode t))
