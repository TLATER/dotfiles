(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:background "#151515"))))
 '(fancy-battery-discharging ((t (:inherit nil :foreground "#599cab")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "/opt/android-sdk")
 '(battery-mode-line-format " %b%p%% ")
 '(battery-status-function (quote battery-linux-sysfs))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "740ccd12c61e3b4dc96b452e662d61598658042dc642e2ce48967d369f5f2930" "d20a27e387c8d961b1c6dfa877859c06fab9b03bfa56350e6c9b61d9e90b1090" "f1d5ef054829b643d2c758bf201f7b1972a0455006b5b42270e2a260c8102c3c" default)))
 '(diredp-image-preview-in-tooltip nil)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(ecb-compilation-buffer-names
   (quote
    (("*Calculator*")
     ("*vc*")
     ("*vc-diff*")
     ("*Apropos*")
     ("*Occur*")
     ("*shell*")
     ("\\*[cC]ompilation.*\\*" . t)
     ("\\*i?grep.*\\*" . t)
     ("*JDEE Compile Server*")
     ("*Help*")
     ("*Completions*")
     ("*Backtrace*")
     ("*Compile-log*")
     ("*bsh*")
     ("*Messages*")
     ("*compilation*"))))
 '(ecb-compile-window-height 10)
 '(ecb-major-modes-show-or-hide (quote (nil)))
 '(ecb-options-version "2.50")
 '(ecb-other-window-behavior (quote smart))
 '(ecb-source-path (quote (("/" "/"))))
 '(ecb-vc-enable-support t)
 '(fancy-battery-mode t)
 '(fancy-battery-show-percentage t)
 '(openwith-associations
   (quote
    (("\\.wav\\'" "aplay"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp3\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\)\\'" "feh"
      (file)))))
 '(openwith-confirm-invocation nil)
 '(openwith-mode t)
 '(org-agenda-files (quote ("~/Documents/org/coursework.org")) t)
 '(same-window-buffer-names (quote ("*Org Agenda*")))
 '(sml/theme (quote respectful))
 '(whitespace-global-modes (quote (not text-aid-too-mode erc-mode))))

;; Add MELPA package repository
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Set font
(set-frame-font "Tamzen:pixelsize=14" nil t)

;; Auto-complete setup
(require 'auto-complete-config)
(ac-config-default)

;; Add nethack
(add-to-list 'load-path "~/.emacs.d/user/nethack")

(add-to-list 'load-path "~/.emacs.d/usr/jde-int/jde-int.el")
(add-to-list 'load-path "~/.emacs.d/user/ecb")
(require 'ecb-autoloads)

;; Add a mode for the chrome extension
(define-derived-mode text-aid-too-mode html-mode
  (setq mode-name "Text-Aid-Too"))
(add-to-list 'auto-mode-alist
             '("tlater-text-aid-too-[-[:alnum:]]*\..*" . text-aid-too-mode))
(add-hook 'text-aid-too-mode-hook (lambda () (flyspell-mode t)))

;; Avoid the annoying help messages
(setq show-help-function nil)

;; Make emacs transparent
(setq frame-alpha-lower-limit 0)
(set-frame-parameter (selected-frame) 'alpha '(90 0))
(add-to-list 'default-frame-alist '(alpha 90 0))

;; Hide hidden files in dired
(require 'dired-x)
(setq dired-omit-files "^\\...+$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Use sml
(sml/setup)

;; Open with external applications
(openwith-mode t)

;; Set folding keybindings
(global-set-key [C-tab] 'folding-toggle-show-hide)

;; Set compile keybindings
(global-set-key (kbd "C-x c") 'compile)

;; Comment region
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)

;; Organisation handler configuration
(setq org-agenda-files '("~/Documents/org/coursework.org"
                         "~/Documents/org/appointments.org"))
(setq org-default-notes-file "~/Documents/org/notes.org")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Enable electric pair mode
(electric-pair-mode)

;; Disable backup/autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Format line numbering (WHOOOOO! Use linum-mode)
(setq linum-format "%3d | ")

;; Mark trailing whitespace and code beyond 80 chars
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; c settings
(setq c-basic-offset 4)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Backtab to delete indentation of current line, improve this!
(global-set-key (kbd "<backtab>") 'unindent-region)

;; Backspace and help fix
(global-set-key [(control h)] 'delete-backward-char)
(global-set-key (kbd "C-?") 'help-command)

;; Don't use actual tabs for indentation
(setq-default indent-tabs-mode nil)

;; Insert an include statement in C files
(defun insert-include (header)
  "Insert include statement"
  (interactive "sLibrary: ")
  (insert "#include <" header ">")
  )

;; Unindents an entire region or just one line if none is active
(defun unindent-region ()
  "Removes the tabs of either the current line or mark"
  (interactive)
  (if (use-region-p)
    (progn
      (setq current-prefix-arg '(-4))
      (call-interactively 'indent-rigidly)
    )
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
                (replace-match "")))))
)

;; Android mode
(require 'android-mode)


;; Disable the obnoxious alarm bell
(setq ring-bell-function 'ignore)

;; IRC client settings
(setq sound-default "/home/tlater/nihaha.wav")

(defun sound (&optional path)
  (start-process-shell-command
   "sound"
   nil
   (concat "aplay -fcd " (or path sound-default))))

(defun erc-message-receive-sound (proc parsed)
  (let* ((tgt (car (erc-response.command-args parsed)))
          (privp (erc-current-nick-p tgt)))
    (and
     privp
     (sound)
     nil)))

(add-hook 'erc-text-matched-hook
          (lambda (match-type nickuserhost message)
            (cond
             ((eq match-type 'currnet-nick)
              (sound)))))

(add-hook 'erc-server-PRIVMSG-functions
          'erc-message-receive-sound)

(global-set-key (kbd "s-c") (lambda ()
                             (interactive)
                             (erc :server "localhost"
                                  :nick "tlater"
                                  :password (read-passwd "Password: "))))

(require 'zone)

;; Set screensaver
(defun lock-screen ()
   "Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
   (interactive)
   (save-excursion
     ;(shell-command "/home/mbax4tm2/.local/bin/pyxtrlock &")
     (set-process-sentinel
      (start-process "xtrlock" nil "/home/mbax4tm2/.local/bin/pyxtrlock")
      '(lambda (process event)
         (zone-leave-me-alone)))
     (zone-when-idle 1)))
