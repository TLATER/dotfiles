;; ------------------------------------------------------------
;; org.el
;;
;; Org configuration
;; ------------------------------------------------------------

;; Set up org
(use-package org
  :init
  (setq org-agenda-files '("~/Documents/org/uni.org"
                           "~/Documents/org/projects.org"
                           "~/Documents/org/official.org"
                           "~/Documents/org/calendar.org"
                           "~/Documents/org/misc.org"))
  (setq org-default-notes-file '("~/Documents/org/notes.org"))
  (setq org-capture-templates
        '(("c" "Calendar entry"
           entry (file "~/Documents/org/calendar.org")
           "* %^{Title}\n  %^T\n  %?\n")))

  :commands org-agenda
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))

(use-package org-mobile-sync
  :init
  (setq org-directory "~/Documents/org")
  (setq org-mobile-inbox-for-pull "~/Documents/org/from-mobile.org")
  (setq org-mobile-directory "~/Owncloud/Org/"))
