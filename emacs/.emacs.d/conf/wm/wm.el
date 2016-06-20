;; ------------------------------------------------------------
;; wm.el
;;
;; EXWM generic configuration
;; ------------------------------------------------------------

;; Load exwm
(use-package exwm
  :init
  (setq exwm-workspace-number 9)

  :config
  ;; Load the theme
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'cyan t)

  ;; Stop the user server and elevate this session to server
  (shell-command "systemctl --user stop emacs")
  (server-start)

  (add-hook 'exwm-update-class-hook
            (lambda ()
              (rename-buffer exwm-class-name t)))

  ;; Load custom functions and keybindings
  (load-file "~/.emacs.d/conf/wm/launch.el")
  (load-file "~/.emacs.d/conf/wm/volume.el")
  (load-file "~/.emacs.d/conf/wm/brightness.el")
  (load-file "~/.emacs.d/conf/wm/keybindings.el")

  ;; Enable the systemtray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (exwm-enable))
