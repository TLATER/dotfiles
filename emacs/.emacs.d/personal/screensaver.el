;; ------------------------------------------------------------
;; screensaver.el
;;
;; Zone-based screensaver configuration
;; ------------------------------------------------------------

(require 'zone)

;; config
(defun lock-screen ()
  "Lock screen using (zone) and xtrlock
   calls M-x zone on all frames and runs xtrlock"
  (interactive)

  (save-excursion
    (set-process-sentinel
     (start-process "xtrlock" nil "/home/mbax4tm2/.local/bin/pyxtrlock")

     '(lambda (process event)
        (zone-leave-me-alone)))
    (zone-when-idle 1)))

;;; js-mode.el ends here
