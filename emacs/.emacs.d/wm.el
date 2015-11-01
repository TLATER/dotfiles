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

(defvar volume-level "The current volume level in %")
(defvar volume-mute-state "The current mute state")

(defun volume-parse-filter (proc string)
  "Parses an amixer message for the current level and mute status"
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)

      ;; Get the current volume and set the status variable
      (string-match "\\[\\([[:digit:]]+\\)%\\]" string)
      (setq volume-level (string-to-number (match-string 1 string)))

      ;; Get the current mute state and set the status variable
      (string-match "\\[\\([[:alpha:]]+\\)\\]" string)
      (setq volume-mute-state (string= "off" (match-string 1 string)))

      ;; Insert the values into the buffer
      (erase-buffer)
      (insert string)
      (volume-display))))

(defun volume-display ()
  "Displays the current volume level"
  (interactive)
  (message (concat "Volume: "
                   (number-to-string volume-level) "%% "
                   (if volume-mute-state "muted"))))

(defun volume-mute (&optional state)
  "Toggles the volume state or sets it to the given value.
When called interactively, a negative prefix turns mute on and a positive off"
  (interactive "P")
  (let ((mute-string))

    (if state

        (if (< 0 state)
            (setq mute-string "on")
          (setq mute-string "off"))

      (setq mute-string "toggle"))

    (start-process "volume" "*Volume*" "amixer" "set" "Master" mute-string)
    (set-process-filter (get-process "volume") 'volume-parse-filter)
    (accept-process-output (get-process "volume"))))

(defun volume-change (amount)
  "Adjusts volume relatively using the amount given"
  (interactive "NChange volume by: ")
  (require 'calc-arith)
  (let ((volume-string))
    (if (< 0 amount)
        (setq volume-string (concat (number-to-string (math-abs amount)) "%+"))
      (setq volume-string (concat (number-to-string (math-abs amount)) "%-")))
    (start-process "volume" "*Volume*" "amixer" "set" "Master" volume-string)
    (set-process-filter (get-process "volume") 'volume-parse-filter)
    (accept-process-output (get-process "volume"))))

(defun brightness-change (amount)
  "Adjust screen brightness relatively using the amount given"
  (interactive "NChange screen brightness by: ")
  (let ((brightness-string))
    (if (> 0 amount)
        (setq brightness-string (concat (number-to-string amount) "%"))
      (setq brightness-string (concat "+" (number-to-string amount) "%")))
    (start-process "brightness" "*Brightness*" "xbacklight" brightness-string)))

;; Load the requirements
(add-to-list 'load-path "~/.emacs.d/user/xelb")
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

;;; Keybindings

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

;; Application launching
(exwm-input-set-key (kbd "s-p") 'launch-application-interactively)
(exwm-input-set-key (kbd "s-<return>") (lambda ()
                                         (interactive)
                                         (launch-application "xterm")))

;; Window switching
(exwm-input-set-key (kbd "s-j") (lambda ()
                                  (interactive)
                                  (other-window 1)))
(exwm-input-set-key (kbd "s-k") (lambda ()
                                  (interactive)
                                  (other-window -1)))

;; Media keys
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (lambda ()
                                                     (interactive)
                                                     (volume-change 5)))
(exwm-input-set-key (kbd "s-<f8>") (lambda ()
                                     (interactive)
                                     (volume-change 5)))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (lambda ()
                                                     (interactive)
                                                     (volume-change -5)))
(exwm-input-set-key (kbd "s-<f7>") (lambda ()
                                     (interactive)
                                     (volume-change -5)))
(exwm-input-set-key (kbd "<XF86AudioMute>") (lambda ()
                                                (interactive)
                                                (volume-mute)))
(exwm-input-set-key (kbd "s-<f6>") (lambda ()
                                     (interactive)
                                     (volume-mute)))
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") (lambda ()
                                                    (interactive)
                                                    (brightness-change 5)))
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda ()
                                                      (interactive)
                                                      (brightness-change -5)))

;; Emacs page-up and down for X windows
(exwm-input-set-simulation-keys
 '(([?\C-v] . next)
   ([?\M-v] . prior)))

;; Enable EXWM
(exwm-enable)

;; Disable the various ill-looking bits
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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
