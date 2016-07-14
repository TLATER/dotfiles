;;; auto-inserts.el---Auto-insert configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 29 Jun 2016
;;
;;; Code:

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.el\\'" . "Elisp skeleton")
     '(
       "Short description: "
       ";;; " (file-name-nondirectory (buffer-file-name)) "---" str \n
       ";;" \n
       ";; Copyright (C) "
       (insert (shell-command-to-string
                "echo -n $(date +%Y)"))
       " "
       (insert user-full-name) \n
       ";; Author: "
       (insert user-full-name) " <" (insert user-mail-address) ">" \n
       ";; Maintainer: "
       (insert user-full-name) " <" (insert user-mail-address) ">" \n
       ";; Created: "
       (insert (shell-command-to-string "echo -n $(date +'%d %b %Y')")) \n
       ";;" \n
       ";;; Code:" \n \n
       > _ \n \n
       ";;; " (file-name-nondirectory (buffer-file-name)) " ends here" \n)))

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.php\\'" . "PHP skeleton")
     '(
       "Short description: "
       "<?php" \n
       "/**" \n
       > " * " str \n
       > " *" \n
       > " * @package " \n
       > " * @author "
       (insert user-full-name) " <" (insert user-mail-address) ">" \n
       > " */" \n \n
       > _ \n
       "?>")))

;;; auto-inserts.el ends here
