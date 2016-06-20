;; ------------------------------------------------------------
;; launch.el
;;
;; EXWM application launching functions
;; ------------------------------------------------------------

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
