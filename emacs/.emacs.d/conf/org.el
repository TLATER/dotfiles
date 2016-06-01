;; ------------------------------------------------------------
;; org.el
;;
;; Org configuration
;; ------------------------------------------------------------

;; Set up org
(use-package org
  :init
  (setq org-agenda-files '("~/Documents/org/coursework.org"))
  (setq org-default-notes-file '("~/Documents/org/notes.org"))
  :defines org-add-link-type
  :commands org-agenda
  :bind
  ("C-c a" . org-agenda))
