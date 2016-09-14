;;; yasnippet.el---YASnippet configuration
;;
;; Copyright (C) 2016 Tristan Daniel Maat
;; Author: Tristan Daniel Maat <tm@tlater.net>
;; Maintainer: Tristan Daniel Maat <tm@tlater.net>
;; Created: 08 Aug 2016
;;
;;; Code:

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  ;; Set the tab keybinding to C-tab to avoid collisions with
  ;; auto-complete.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

  (yas-global-mode 1))

;;; yasnippet.el ends here
