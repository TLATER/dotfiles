;; ------------------------------------------------------------
;; org.el
;;
;; Org configuration
;; ------------------------------------------------------------

;; Set up org
(use-package org
  :init
  (setq org-agenda-files '("~/Documents/org/coursework.org"
                           "~/Documents/org/calendar.org"))
  (setq org-default-notes-file '("~/Documents/org/notes.org"))
  (setq org-capture-templates
        '(("c" "Calendar entry"
           entry (file "~/Documents/org/calendar.org")
           "* %^{Title}\n  %^T\n  %?\n")))

  :commands org-agenda
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))
