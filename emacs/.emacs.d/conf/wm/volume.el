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
