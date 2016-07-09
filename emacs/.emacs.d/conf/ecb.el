;; ------------------------------------------------------------
;;ecb.el
;;
;; ECB configuration
;; ------------------------------------------------------------

;; Set up ecb
(use-package ecb
  :init
  (setq ecb-compile-window-height 10)
  (setq ecb-major-modes-show-or-hide '(nil))
  (setq ecb-other-window-behavior 'smart)
  (setq ecb-source-path '(("/" "/")))
  (setq ecb-vc-enable-support t)
  (setq ecb-compilation-buffer-names
        '(("*Calculator*")
          ("*vc*")
          ("*vc-diff*")
          ("*Apropos*")
          ("*Occur*")
          ("*shell*")
          ("\\*[cC]ompilation.*\\*" . t)
          ("\\*i?grep.*\\*" . t)
          ("*JDEE Compile Server*")
          ("*Help*")
          ("*Completions*")
          ("*Backtrace*")
          ("*Compile-log*")
          ("*bsh*")
          ("*Messages*")
          ("*compilation*")))
  :bind
  ("C-c w r" . ecb-redraw-layout))
