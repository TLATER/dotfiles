;; ------------------------------------------------------------
;; battery-mode.el
;;
;; Battery mode configuration
;; ------------------------------------------------------------

(use-package battery
  :init
  (setq battery-mode-line-format " %b%p%% ")
  (setq battery-status-function (quote battery-linux-sysfs))
  :config
  (display-battery-mode t))
