(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(erc-log-channels-directory "~/.emacs.d/erc/logs/")
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp spelling track truncate)))
 '(erc-scrolltobottom-mode t)
 '(eshell-prefer-lisp-functions t)
 '(flycheck-python-pylint-executable "/usr/bin/pylint3")
 '(helm-dash-browser-func (quote eww))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -interaction nonstopmode -output-directory %o %f" "pdflatex -interaction nonstopmode -output-directory %o %f")))
 '(package-selected-packages
   (quote
    (helm-purpose window-purpose indium stumpwm-mode pytest company-jedi jedi-core exwm mocha company-tern js2-refactor jdee yaml-mode web-mode scss-mode geiser company-auctex cdlatex auctex json-mode js2-mode rainbow-mode elisp-slime-nav rainbow-delimiters company helm-ag helm-descbinds helm-projectile helm smex ido-ubiquitous flx-ido zop-to-char zenburn-theme which-key volatile-highlights undo-tree smartrep smartparens smart-mode-line operate-on-number move-text magit projectile ov imenu-anywhere guru-mode grizzl god-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major dash crux browse-kill-ring beacon anzu ace-window)))
 '(pytest-global-name "py.test-3")
 '(rainbow-x-colors-major-mode-list
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode c-mode c++-mode java-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-default-face ((t nil))))
