;; ------------------------------------------------------------
;; keybindings.el
;;
;; Generic emacs keybindings
;; ------------------------------------------------------------

;; Backtab
(global-set-key (kbd "<backtab>") 'unindent-region)

;; Compilation
(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x r") 'recompile)

;; Commenting
(global-set-key (kbd "C-;") 'comment-region)
;; This is C-S-;
(global-set-key (kbd "C-:") 'uncomment-region)
