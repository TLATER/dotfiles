;; ------------------------------------------------------------
;; auto-inserts.el
;;
;; Emacs auto inserts
;; ------------------------------------------------------------

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.el\\'" . "Elisp skeleton")
     '(
       "Short description: "
       ";; ------------------------------------------------------------" \n
       ";; " (file-name-nondirectory (buffer-file-name)) \n
       ";;" \n
       ";; " str \n
       ";; ------------------------------------------------------------" \n \n
       > _ \n)))
