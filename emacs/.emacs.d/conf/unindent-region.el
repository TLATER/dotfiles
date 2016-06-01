;; ------------------------------------------------------------
;; unindent-region.el
;;
;; A function to unindent a region
;; ------------------------------------------------------------

;; Unindents an entire region or just one line if none is active
(defun unindent-region ()
  "Removes the tabs of either the current line or mark"
  (interactive)
  (if (use-region-p)
    (progn
      (setq current-prefix-arg '(-4))
      (call-interactively 'indent-rigidly)
    )
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
                (replace-match "")))))
)
