;;; org-gcal.el---Google Calendar synchronization settings
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 31 Aug 2016
;;
;;; Code:

(use-package org-gcal
  :init
  (setq org-gcal-client-id "304016141670-lgmfcfegrslc1q6ptkc36mer5umg3up5.apps.googleusercontent.com"
        org-gcal-client-secret "ckdavAitFUOyfjZNI5hhAw1A"
        org-gcal-file-alist '(("tris.maat@gmail.com" . "~/Documents/org/calendar.org")
                              ("12dhl92effmsru3q8jecokk0l0@group.calendar.google.com" . "~/Documents/org/timetable.org")))
  :bind
  ("C-c s" . org-gcal-sync))

;;; org-gcal.el ends here
