;;; js.el --- JS configuration                       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tristan Daniel Maat

;; Author: Tristan Daniel Maat <mbax4tm2@E-C07KI1803.it.manchester.ac.uk>
;; Keywords: languages, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; js2
;; Already required by prelude.
;; init
(setq js2-basic-offset 4)
(setq-default js2-additional-externs
              '("$"
                "define"
                "require"))

;; js2-refactor
(prelude-require-package 'js2-refactor)
;; config
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")

;; (prelude-require-package 'indium)

;; ;; tern
;; (add-to-list 'load-path "~/.emacs.d/.tern/emacs")
;; (autoload 'tern-mode "tern.el" nil t)
;; (add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; ;; company-tern
;; (prelude-require-package 'company-tern)
;; ;; config
;; (add-to-list 'company-backends 'company-tern)

;; mocha
;; (prelude-require-package 'mocha)
;; ;; config
;; (define-key js2-mode-map (kbd "C-c t .") 'mocha-test-at-point)
;; (define-key js2-mode-map (kbd "C-c t a") 'mocha-test-project)
;; (define-key js2-mode-map (kbd "C-c t m") 'mocha-test-file)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;; ember mode
;; (add-hook 'js-mode-hook (lambda () (ember-mode t)))
;; (add-hook 'web-mode-hook (lambda () (ember-mode t)))

(provide 'js)
;;; js.el ends here
