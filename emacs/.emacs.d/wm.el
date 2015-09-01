;; Load the requirements
(add-to-list 'load-path "~/.emacs.d/user/xelb")
(add-to-list 'load-path "~/.emacs.d/user/xelb/lib")
(add-to-list 'load-path "~/.emacs.d/user/xelb/util")
(add-to-list 'load-path "~/.emacs.d/user/exwm")

;; Load the theme (since this is an X session...)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'cyan t)

;; Stop the user server and elevate this session to server
(shell-command "systemctl --user stop emacs")
(server-start)

;; EXWM settings
(require 'exwm)
;; Use class name to name an EXWM buffer
(add-hook 'exwm-update-class-hook
          (lambda () (rename-buffer exwm-class-name t)))
;; Set the number of workspaces to 9
(setq exwm-workspace-number 9)
;; Enable EXWM
(exwm-enable)

;; WM-specific helper functions
(defun launch-application-interactively ()
  "Prompts the user to launch an application interactively."
  (interactive)
  (let ((application))
    (setq application (read-shell-command "Launch application: "))
    (launch-application application)))

(defun launch-application (command)
  "Launches an application asynchronously."
  (let ((name))
    (setq command (split-string-and-unquote command))

    (setq name (car command))
    (apply 'start-process name nil command)))

;; Disable the various ill-looking bits
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Keybindings
;; Back to line-mode
(exwm-input-set-key (kbd "s-r") 'exwm-reset)
;; Workspace keybindings
(exwm-input-set-key (kbd "s-1")
                    (lambda ()
                      (interactive)
                      (message "Workspace 0")
                      (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-2")
                    (lambda ()
                      (interactive)
                      (message "Workspace 1")
                      (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-3")
                    (lambda ()
                      (interactive)
                      (message "Workspace 2")
                      (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-4")
                    (lambda ()
                      (interactive)
                      (message "Workspace 3")
                      (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-5")
                    (lambda ()
                      (interactive)
                      (message "Workspace 4")
                      (exwm-workspace-switch 4)))
(exwm-input-set-key (kbd "s-6")
                    (lambda ()
                      (interactive)
                      (message "Workspace 5")
                      (exwm-workspace-switch 5)))
(exwm-input-set-key (kbd "s-7")
                    (lambda ()
                      (interactive)
                      (message "Workspace 6")
                      (exwm-workspace-switch 6)))
(exwm-input-set-key (kbd "s-8")
                    (lambda ()
                      (interactive)
                      (message "Workspace 7")
                      (exwm-workspace-switch 7)))
(exwm-input-set-key (kbd "s-9")
                    (lambda ()
                      (interactive)
                      (message "Workspace 8")
                      (exwm-workspace-switch 8)))
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

;; Application launching
(exwm-input-set-key (kbd "s-p") 'launch-application-interactively)
(exwm-input-set-key (kbd "s-<return>") (lambda ()
                                         (interactive)
                                         (launch-application "xterm")))

;; Additional UI
(setq display-time-default-load-average nil)
(display-time-mode t)

;; Autostart
;; (exwm-workspace-switch (1- exwm-workspace-number))
;; (launch-application "thunderbird")
;; (launch-application "apulse32 skype")
;; (exwm-workspace-switch 0)

;; Restart the server when exiting
;;(add-hook 'kill-emacs-hook (lambda ()
;;                             (shell-command "systemctl --user start emacs")))
