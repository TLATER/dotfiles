(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fancy-battery-discharging ((t (:inherit nil :foreground "#599cab")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " %b%p%% ")
 '(battery-status-function (quote battery-linux-sysfs))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "740ccd12c61e3b4dc96b452e662d61598658042dc642e2ce48967d369f5f2930" "d20a27e387c8d961b1c6dfa877859c06fab9b03bfa56350e6c9b61d9e90b1090" "f1d5ef054829b643d2c758bf201f7b1972a0455006b5b42270e2a260c8102c3c" default)))
 '(diredp-image-preview-in-tooltip nil)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(fancy-battery-mode t)
 '(fancy-battery-show-percentage t)
 '(openwith-associations
   (quote
    (("\\.wav\\'" "aplay"
      (file))
     ("\\.pdf\\'" "evince"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp3\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\)\\'" "feh"
      (file)))))
 '(openwith-confirm-invocation nil)
 '(openwith-mode t)
 '(same-window-buffer-names (quote ("*Org Agenda*")))
 '(sml/theme (quote respectful)))

;; Add MELPA package repository
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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
(global-set-key (kbd "C-x r") 'recompile)

;; Comment region
(global-set-key (kbd "C-;") 'comment-region)
(global-set-key (kbd "C-:") 'uncomment-region)

;; Organisation handler configuration
(setq org-agenda-files '("~/Documents/notes"))
(setq org-default-notes-file "~/Documents/notes/life.org")

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote(("t" "todo" entry (file+headline "~/Documents/notes/tasks.org"
                                              "Miscellaneous tasks")
              "** TODO %?")

             ("u" "urgent" entry (file+headline "~/Documents/notes/tasks.org"
                                                "Urgent tasks")
              "** TODO %?")

             ("n" "note" entry (file "~/Documents/notes/misc.org")
              "* %?\n  :NOTE:\n  \n")

             ("a" "anime" entry (file+headline "~/Documents/notes/anime.org"
                                               "Found on the internet")
              "** %?")
             )))

(setq org-agenda-custom-commands
      '(("N" "List all notes" tags "+NOTE")
        ("n" "Agenda and all TODO's + notes"
         ((agenda "")
          (alltodo "")
          (tags "+NOTE")))
        ))

;; Improved latex commands
;;(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;; Auto-complete setup
(ac-config-default)

;; Fix ls
;;(setenv "PATH" (concat (getenv "PATH") ":/bin:/opt/android-sdk/tools"))
;;(setq exec-path (append exec-path '("/bin")))

;; Android development config
;;(require 'android-mode)

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
