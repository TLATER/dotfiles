;;; eww-rdrview.el --- rdrview-based web browsing      -*- lexical-binding: t; -*-

;; Keywords: convenience

;;; Commentary:

;; See https://jiewawa.me/2024/04/another-way-of-integrating-mozilla-readability-in-emacs-eww/

;;; Code:

(require 'eww)

(defun eww-rdrview-toggle-and-reload ()
  "Toggle `eww-rdrview-mode' and reload page in current eww buffer."
  (interactive)
  (if eww-rdrview-mode (eww-rdrview-mode -1)
    (eww-rdrview-mode 1))
  (eww-reload))

(define-key eww-mode-map (kbd "C-c R") 'eww-rdrview-toggle-and-reload)

(defun eww-rdrview-update-title ()
  "Change title key in `eww-data' with first line of buffer.

   It should be the title of the web page as returned by `rdrview'"
  (save-excursion
    (goto-char (point-min))
    (if-let* ((title (thing-at-point 'line t)))
        (plist-put eww-data :title (string-trim title))))
  (eww--after-page-change))

(define-minor-mode eww-rdrview-mode
  "Toggle whether to use `rdrview' to make eww buffers more readable."
  :lighter " rdrview"
  (if eww-rdrview-mode
      (progn
        (setq eww-retrieve-command '("rdrview" "-T" "title,sitename,body" "-H"))
        (add-hook 'eww-after-render-hook #'eww-rdrview-update-title))
    (progn
      (setq eww-retrieve-command nil)
      (remove-hook 'eww-after-render-hook #'eww-rdrview-update-title))))

(provide 'eww-rdrview)

;;; eww-rdrview.el ends here
