;;; notmuch.el---notmuch configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 09 Jul 2016
;;
;;; Code:

(use-package notmuch
  :commands notmuch-search-tag notmuch-search-get-tags toggle-notmuch-search-tags
  :functions org-open-at-point
  :config
  (defun toggle-notmuch-search-tags (&rest tags)
    "Toggle the given tags"
    (interactive)
    (dolist (e tags)
      (if (member e (notmuch-search-get-tags))
          (notmuch-search-tag (list (concat "-" e)))
        (notmuch-search-tag (list (concat "+" e))))))

  ;; Search mode
  (define-key notmuch-search-mode-map "D"
    (lambda ()
      "toggle deleted"
      (interactive)
      (notmuch-search-tag (list "-inbox"))
      (notmuch-search-tag (list "-unread"))
      (toggle-notmuch-search-tags "deleted")))

  (define-key notmuch-search-mode-map "S"
    (lambda ()
      "toggle seen"
      (interactive)
      (toggle-notmuch-search-tags "unread")))

  (define-key notmuch-search-mode-map "J"
    (lambda ()
      "toggle junk"
      (interactive)
      (notmuch-search-tag (list "-inbox"))
      (notmuch-search-tag (list "-unread"))
      (toggle-notmuch-search-tags "spam")))

  (define-key notmuch-search-mode-map "I"
    (lambda ()
      "toggle inbox"
      (interactive)
      (toggle-notmuch-search-tags "inbox")))

  ;; Show mode
  (define-key notmuch-show-mode-map "O"
    (lambda ()
      "Open at point."
      (interactive)
      (org-open-at-point nil ))))

;;; notmuch.el ends here
