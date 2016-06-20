;; ------------------------------------------------------------
;; brightness.el
;;
;; EXWM brightness functions
;; ------------------------------------------------------------

(defun brightness-change (amount)
  "Adjust screen brightness relatively using the amount given"
  (interactive "NChange screen brightness by: ")
  (let ((brightness-string))
    (if (> 0 amount)
        (setq brightness-string (concat (number-to-string amount) "%"))
      (setq brightness-string (concat "+" (number-to-string amount) "%")))
    (start-process "brightness" "*Brightness*" "xbacklight" brightness-string)))
