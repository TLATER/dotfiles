;; ------------------------------------------------------------
;; launch.el
;;
;; EXWM application launching functions
;; ------------------------------------------------------------

(defvar launch-aliases '(("chromium-browser" . "chromium-browser --start-fullscreen")
                         ("chromium" . "chromium --start-fullscreen")))

(defun launch-application-interactively ()
  "Prompts the user to launch an application interactively."
  (interactive)
  (let ((application))
    (setq application (read-shell-command "Launch application: "))

    ;; For aliases to work, we need to remove trailing spaces
    (launch-application (replace-regexp-in-string " $" "" application))))

(defun launch-application (command)
  "Launches an application asynchronously."

  (let ((name) (alias))
    ;; Apply any aliases for the current command
    (setq alias (cdr (assoc-string command launch-aliases)))
    (if alias
        (setq command alias))

    (setq command (split-string-and-unquote command))

    (setq name (car command))
    (apply 'start-process name nil command)))
