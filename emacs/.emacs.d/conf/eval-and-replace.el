;; ------------------------------------------------------------
;; eval-and-replace.el
;;
;; A function to evaluate the preceding sexp and replace it with its
;; value (e.g. (+ 5 5) -> 10)
;; ------------------------------------------------------------

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
