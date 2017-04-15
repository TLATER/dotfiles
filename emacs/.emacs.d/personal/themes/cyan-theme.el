(defgroup cyan nil
  "An Emacs theme"
  :prefix "cyan-")

(deftheme cyan "A theme")

(defcustom cyan-tty-extended-palette nil
  "Use the extended 256-color palette in the terminal?
When t, assume a regular 256-color palette, otherwise assume a
customized 16-color palette."
  :type '(boolean (const :tag "16 colors" nil)
                  (const :tag "256 colors" t))
  :group 'cyan)

(defvar cyan-color-alist
  `((base0   "#0f0f0f" ,(if cyan-tty-extended-palette "color-232" "black"))
    (base1   "#11151c" ,(if cyan-tty-extended-palette "color-233" "brightblack"))
    (base2   "#091f2e" ,(if cyan-tty-extended-palette "color-17"  "brightgreen"))
    (base3   "#0a3749" ,(if cyan-tty-extended-palette "color-18"  "brightblue"))
    (base4   "#245361" ,(if cyan-tty-extended-palette "color-24"  "brightyellow"))
    (base5   "#268bd2" ,(if cyan-tty-extended-palette "color-81"  "brightcyan"))
    (base6   "#99d1ce" ,(if cyan-tty-extended-palette "color-122" "white"))
    (base7   "#d3ebe9" ,(if cyan-tty-extended-palette "color-194" "brightwhite"))

    (red     "#dc322f" ,(if cyan-tty-extended-palette "color-124" "red"))
    (orange  "#d26937" ,(if cyan-tty-extended-palette "color-166" "brightred"))
    (yellow  "#b58900" ,(if cyan-tty-extended-palette "color-214" "yellow"))
    (magenta "#707880" ,(if cyan-tty-extended-palette "color-67"  "brightmagenta"))
    (violet  "#4e5166" ,(if cyan-tty-extended-palette "color-60"  "magenta"))
    (blue    "#195466" ,(if cyan-tty-extended-palette "color-24"  "blue"))
    (cyan    "#599cab" ,(if cyan-tty-extended-palette "color-44"  "cyan"))
    (green   "#2aa889" ,(if cyan-tty-extended-palette "color-78"  "green")))
  "List of colors the theme consists of.")

(defun cyan-set-faces (faces)
  "Helper function that sets the faces of the theme to a list of FACES.
See `cyan-transform-face' for the transformation, see
`cyan-transform-spec' for the rules."
  (apply #'custom-theme-set-faces 'cyan
         (mapcar #'cyan-transform-face faces)))

(defun cyan-transform-face (face)
  "Helper function that transforms a FACE to all variants.
FACE is a list where the first element is the name of the
affected face and the remaining elements specify the face
attributes which are transformed into face attributes for both
graphical and terminal displays.  See `cyan-transform-spec' for
the rules that are applied to the face attributes."
  (let* ((name (car face))
         (spec (cdr face))
         (graphic-spec (cyan-transform-spec spec 'graphic))
         (tty-spec (cyan-transform-spec spec 'tty)))
    `(,name ((((type graphic)) ,@graphic-spec)
             (((type tty)) ,@tty-spec)))))

(defun cyan-transform-spec (spec display)
  "Helper function that transforms SPEC for DISPLAY.
DISPLAY is either 'graphic or 'tty, SPEC is a property list where
the values are substituted with colors from `cyan-color-alist'
depending on DISPLAY for keys which are either :foreground or
:background.  All other key-value combinations remain unchanged."
  (let (output)
    (while spec
      (let* ((key (car spec))
             (value (cadr spec))
             (index (cond ((eq display 'graphic) 1)
                          ((eq display 'tty) 2)))
             (color (nth index (assoc value cyan-color-alist))))
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output
                               (list key (cyan-transform-spec value display)))))
         ((and (memq key '(:foreground :background :underline :overline :color))
               color)
          (setq output (append output (list key color))))
         (t (setq output (append output (list key value))))))
      (setq spec (cddr spec)))
    output))

(cyan-set-faces
 '((default :foreground base6 :background base0)
   (button :foreground base4 :box t)
   (shadow :foreground base4)
   (highlight :background base2)
   (link :foreground orange :underline t)
   (link-visited :foreground yellow)
   (cursor :background base6)
   (region :foreground unspecified :background base3)
   (secondary-selection :foreground unspecified :background violet)
   (hl-line :background base1)
   (linum :foreground base4 :background base1)
   (fringe :foreground base6 :background base1)
   (vertical-border :foreground base4)
   (tooltip :foreground base6 :background base0)
   (trailing-whitespace :background red)

   ;; font-lock
   (escape-glyph :foreground orange :weight bold)
   (font-lock-builtin-face :foreground orange)
   (font-lock-comment-face :foreground base4)
   (font-lock-comment-delimiter-face :foreground base4)
   (font-lock-constant-face :foreground cyan :weight bold)
   (font-lock-doc-face :foreground green)
   (font-lock-function-name-face :foreground base5)
   (font-lock-keyword-face :foreground blue :weight bold)
   (font-lock-negation-char-face :foreground red)
   (font-lock-preprocessor-face :foreground red)
   (font-lock-regexp-grouping-construct :foreground yellow)
   (font-lock-regexp-grouping-backslash :foreground yellow)
   (font-lock-string-face :foreground green)
   (font-lock-type-face :foreground orange)
   (font-lock-variable-name-face :foreground base5)
   (font-lock-warning-face :foreground red)
   (error :foreground red)
   (success :foreground green)
   (warning :foreground orange)

   ;; search and highlighting
   (match :background base5)
   (isearch :inverse-video t)
   (isearch-fail :foreground red)
   (lazy-highlight :foreground base2 :background yellow)

   ;; mode and header lines
   (minibuffer-prompt :foreground cyan)
   (header-line :foreground base5 :background base2)
   (menu :background base3 :foreground base6)
   (mode-line :foreground base5 :background base2 :box nil)
   (mode-line-inactive :foreground base4 :background base1 :box nil)
   (mode-line-highlight :foreground base6)
   (mode-line-buffer-id :weight bold)

   ;; customize
   (custom-button :foreground base4 :box t)
   (custom-button-mouse :foreground base5 :box t)
   (custom-group-tag :inherit fixed-pitch :foreground magenta)
   (custom-state :foreground cyan)

   ;; compilation
   (compilation-mode-line-fail :foreground unspecified :inherit compilation-error)
   (compilation-mode-line-exit :foreground unspecified :inherit compilation-info)

   ;; diff
   (diff-added :foreground green)
   (diff-changed :foreground cyan)
   (diff-header :foreground yellow)
   (diff-file-header :weight bold)
   (diff-refine-added :weight bold)
   (diff-refine-change :weight bold)
   (diff-refine-removed :weight bold)
   (diff-removed :foreground red)

   ;; erc
   (erc-current-nick-face :foreground base5)
   (erc-dangerous-host-face :foreground red)
   (erc-direct-msg-face :foreground orange)
   (erc-error-face :inherit error)
   (erc-fool-face :inherit shadow)
   (erc-header-line :foreground base2 :background magenta)
   (erc-input-face :inherit default)
   (erc-inverse-face :inverse-video t)
   (erc-keyword-face :foreground green)
   (erc-my-nick-face :foreground yellow)
   (erc-nick-msg-face :foreground orange)
   (erc-notice-face :foreground cyan)
   (erc-pal-face :foreground yellow)
   (erc-prompt-face :foreground orange)
   (erc-timestamp-face :foreground magenta :weight bold)

   ;; ediff
   ;; FIXME add support for threeway merges (*-diff-ancestor)
   (ediff-current-diff-A :foreground red :background base2)
   (ediff-current-diff-B :foreground green :background base2)
   (ediff-current-diff-C :foreground blue :background base2)
   ;(ediff-current-diff-Ancestor)
   (ediff-even-diff-A :background base2)
   (ediff-even-diff-B :background base2)
   (ediff-even-diff-C :background base2)
   ;(ediff-even-diff-Ancestor)
   (ediff-fine-diff-A :foreground red :background base3)
   (ediff-fine-diff-B :foreground green :background base3)
   (ediff-fine-diff-C :foreground cyan :background base3)
   ;(ediff-fine-diff-Ancestor)
   (ediff-odd-diff-A :background base2)
   (ediff-odd-diff-B :background base2)
   (ediff-odd-diff-C :background base2)
   ;(ediff-odd-diff-Ancestor)

   ;; eshell
   (eshell-prompt :foreground yellow :weight bold)
   (eshell-ls-archive :foreground magenta)
   (eshell-ls-backup :foreground violet :weight bold)
   (eshell-ls-clutter :foreground base5)
   (eshell-ls-directory :foreground cyan :weight bold)
   (eshell-ls-executable :foreground green)
   (eshell-ls-missing :foreground red :weight bold)
   (eshell-ls-product :foreground base3)
   (eshell-ls-readonly :foreground red)
   (eshell-ls-special :foreground orange :weight bold)
   (eshell-ls-symlink :foreground blue :weight bold)
   (eshell-ls-unreadable :foreground red)

   ;; elfeed
   (elfeed-search-feed-face :foreground orange)
   (elfeed-search-tag-face :foreground green)
   (elfeed-search-title-face :foreground base6)
   (elfeed-search-title-unread-face :foreground base6 :weight bold)
   (elfeed-search-date-face :foreground violet)

   ;; flymake
   (flymake-errline :underline (:style wave :color red))
   (flymake-warnline :underline (:style wave :color orange))

   ;; flyspell
   (flyspell-duplicate :underline (:style wave :color orange))
   (flyspell-incorrect :underline (:style wave :color red))

   ;; ido
   (ido-first-match :foreground yellow :weight bold)
   (ido-indicator :foreground red)
   (ido-only-match :foreground green)
   (ido-subdir :foreground red)

   ;; makefile
   (makefile-space :background magenta)

   ;; outline
   (outline-1 :foreground red)
   (outline-2 :foreground cyan)
   (outline-3 :foreground orange)
   (outline-4 :foreground green)
   (outline-5 :foreground red)
   (outline-6 :foreground cyan)
   (outline-7 :foreground orange)
   (outline-8 :foreground green)

   ;; pulse
   (pulse-highlight-start-face :background base4)

   ;; rcirc
   (rcirc-bright-nick :foreground base7)
   (rcirc-dim-nick :inherit shadow)
   (rcirc-my-nick :foreground base5)
   (rcirc-nick-in-message :inherit rcirc-my-nick)
   (rcirc-other-nick :foreground magenta)
   (rcirc-prompt :foreground orange)
   (rcirc-server :foreground cyan)
   (rcirc-url :inherit link)

   ;; semantic
   (semantic-complete-inline-face :underline base5)
   (semantic-decoration-on-fileless-includes :foreground base4 :background yellow)
   (semantic-decoration-on-private-members-face :background base1)
   (semantic-decoration-on-protected-members-face :background base1)
   (semantic-decoration-on-unknown-includes :background red)
   (semantic-decoration-on-unparsed-includes :background orange)
   (semantic-highlight-edits-face :inherit highlight)
   (semantic-highlight-func-current-tag-face :inherit highlight)
   (semantic-tag-boundary-face :overline cyan)
   (semantic-unmatched-syntax-face :underline red)
   (senator-momentary-highlight-face :inherit highlight)
   (srecode-field-face :underline green)

   ;; speedbar
   (speedbar-button-face :foreground base5)
   (speedbar-directory-face :foreground base5 :weight bold)
   (speedbar-file-face :foreground green)
   (speedbar-highlight-face :inherit highlight)
   (speedbar-selected-face :inherit highlight)
   (speedbar-separator-face :background blue :overline t)
   (speedbar-tag-face :foreground orange)

   ;; show-paren-mode
   (show-paren-match :foreground base2 :background orange :inverse-video nil)
   (show-paren-mismatch :foreground base2 :background red :inverse-video nil)

   ;; term
   (term-color-black :foreground base1 :background base1)
   (term-color-red :foreground red :background red)
   (term-color-green :foreground green :background green)
   (term-color-yellow :foreground yellow :background yellow)
   (term-color-blue :foreground blue :background blue)
   (term-color-magenta :foreground magenta :background magenta)
   (term-color-cyan :foreground cyan :background cyan)
   (term-color-white :foreground base6 :background base6)
   (term-default-fg-color :inherit term-color-white)
   (term-default-bg-color :inherit term-color-black)

   ;; widget
   (widget-button-pressed :foreground red)
   (widget-documentation :foreground green)
   (widget-field :background base4)
   (widget-single-line-field :inherit widget-field)

   ;; which-func-mode
   (which-func :foreground orange)

   ;; whitespace-mode
   (whitespace-empty :foreground base7 :background cyan)
   (whitespace-hspace :foreground base7 :background magenta)
   (whitespace-indentation :background yellow)
   (whitespace-line :foreground base6 :background violet)
   (whitespace-newline :foreground base3)
   (whitespace-space :foreground base7 :background base2)
   (whitespace-space-after-tab :foreground base7 :background orange)
   (whitespace-space-before-tab :foreground base7 :background orange)
   (whitespace-tab :background base3)
   (whitespace-trailing :foreground base7 :background red)

   ;; external packages

   ;; ace-jump
   (ace-jump-face-foreground :foreground red :background unspecified)
   (ace-jump-face-background :foreground base4 :background unspecified)

   ;; auctex
   (font-latex-bold-face :inherit bold)
   (font-latex-doctex-documentation-face :inherit highlight)
   (font-latex-doctex-preprocessor-face :inherit highlight :foreground red)
   (font-latex-italic-face :inherit italic)
   (font-latex-math-face :foreground base5)
   (font-latex-sectioning-0-face :inherit bold :foreground yellow)
   (font-latex-sectioning-1-face :inherit bold :foreground yellow)
   (font-latex-sectioning-2-face :inherit bold :foreground yellow)
   (font-latex-sectioning-3-face :inherit bold :foreground yellow)
   (font-latex-sectioning-4-face :inherit bold :foreground yellow)
   (font-latex-sectioning-5-face :inherit bold :foreground yellow)
   (font-latex-sedate-face :foreground orange)
   (font-latex-slide-title-face :inherit bold :foreground orange)
   (font-latex-string-face :inherit font-lock-string-face)
   (font-latex-verbatim-face :inherit font-lock-string-face)
   (font-latex-warning-face :inherit warning)

   ;; auto-complete
   (ac-completion-face :foreground base7 :background base4)

   ;; circe
   (circe-fool-face :inherit shadow)
   (circe-highlight-nick-face :foreground base5 :weight bold)
   (circe-prompt-face :foreground orange)
   (circe-server-face :foreground cyan)
   (circe-topic-diff-new-face :foreground base2 :background green)
   (circe-topic-diff-removed-face :foreground base0 :background red)

   ;; company
   (company-echo-common :foreground red)
   (company-preview :inherit company-tooltip-selection)
   (company-preview-common :inherit company-preview :foreground orange)
   (company-preview-search :inherit company-preview)
   (company-scrollbar-bg :background base2)
   (company-scrollbar-fg :background base4)
   (company-tooltip :foreground base6 :background base2)
   (company-tooltip-annotation :inherit company-tooltip :foreground red)
   (company-tooltip-common :background base2 :weight bold)
   (company-tooltip-common-selection :foreground base7 :background base4 :weight bold)
   (company-tooltip-mouse :foreground base7 :background base4)
   (company-tooltip-selection :foreground base7 :background base4)

   ;; ecb
   (ecb-default-highlight-face :background violet)
   (ecb-method-non-semantic-face :foreground orange)
   (ecb-mode-line-prefix-face :foreground green)
   (ecb-tag-header-face :foreground base2 :background cyan)
   (ecb-tree-guide-line-face :foreground base4)
   (ecb-type-tag-group-face :foreground magenta)

   ;; flycheck
   (flycheck-error :underline (:style wave :color red))
   (flycheck-info :underline (:style wave :color green))
   (flycheck-warning :underline (:style wave :color orange))

   ;; enh-ruby-mode
   (enh-ruby-heredoc-delimiter-face :foreground green :weight bold)
   (enh-ruby-op-face :foreground magenta)
   (enh-ruby-regexp-face :foreground green)
   (enh-ruby-regexp-delimiter-face :foreground green :weight bold)
   (enh-ruby-string-delimiter-face :foreground green)
   (erm-syn-errline :foreground red)
   (erm-syn-warnline :foreground orange)

   ;; helm
   (helm-bookmark-addressbook :foreground orange)
   (helm-bookmark-file :foreground cyan)
   (helm-bookmark-gnus :foreground magenta)
   (helm-bookmark-info :foreground green)
   (helm-bookmark-man :foreground violet)
   (helm-bookmark-w3m :foreground yellow)
   (helm-buffer-directory :weight bold)
   (helm-buffer-not-saved :foreground red)
   (helm-buffer-process :foreground orange)
   (helm-buffer-saved-out :foreground base7 :background red)
   (helm-buffer-size :foreground magenta)
   (helm-candidate-number :background base3)
   (helm-ff-directory :weight bold)
   (helm-ff-executable :forground green)
   (helm-ff-invalid-symlink :foreground base2 :background red)
   (helm-ff-prefix :foreground base2 :background yellow)
   (helm-ff-symlink :foreground orange)
   (helm-grep-file :foreground cyan)
   (helm-grep-finish :foreground green)
   (helm-grep-lineno :foreground orange)
   (helm-grep-match :foreground yellow)
   (helm-grep-running :foreground red)
   (helm-history-remote :foreground orange)
   (helm-locate-finish :foreground green)
   (helm-match :foreground yellow)
   (helm-moccur-buffer :foreground cyan)
   (helm-prefarg :foreground green)
   (helm-resume-need-update :foreground base7 :background red)
   (helm-selection :inherit highlight)
   (helm-selection-line :inherit highlight)
   (helm-separator :inherit shadow)
   (helm-source-header :foreground base2 :background blue :weight bold)
   (helm-time-zone-current :foreground green)
   (helm-time-zone-home :foreground red)
   (helm-visible-mark :inherit secondary-selection)

   ;; hydra
   (hydra-face-red :foreground red :bold t)
   (hydra-face-blue :foreground cyan :bold t)
   (hydra-face-teal :foreground green :bold t)
   (hydra-face-pink :foreground orange :bold t)
   (hydra-face-amaranth :foreground magenta :bold t)

   ;; jabber
   (jabber-activity-face :foreground green)
   (jabber-activity-personal-face :foreground cyan)
   (jabber-chat-error :inherit error)
   (jabber-chat-prompt-foreign :foreground green)
   (jabber-chat-prompt-local :foreground orange)
   (jabber-chat-prompt-system :foreground cyan)
   (jabber-rare-time-face :foreground magenta :weight bold)
   (jabber-roster-user-away :foreground violet)
   (jabber-roster-user-chatty :foreground yellow)
   (jabber-roster-user-dnd :foreground orange)
   (jabber-roster-user-error :foreground red)
   (jabber-roster-user-offline :inherit shadow)
   (jabber-roster-user-online :foreground cyan)
   (jabber-roster-user-xa :foreground magenta)

   ;; js2-mode
   (js2-error :inherit error)
   (js2-external-variable :foreground orange)
   (js2-function-param :foreground green)
   (js2-instance-member :foreground magenta)
   (js2-jsdoc-html-tag-delimiter :foreground green)
   (js2-jsdoc-html-tag-name :foreground yellow)
   (js2-jsdoc-tag :foreground cyan)
   (js2-jsdoc-type :foreground blue)
   (js2-jsdoc-value :foreground violet)
   (js2-private-function-call :foreground yellow)
   (js2-private-member :foreground orange)
   (js2-warning :underline (:style wave :color orange))

   ;; linum-relative
   (linum-relative-current-face :background base3 :foreground cyan :weight bold)

   ;; lui
   (lui-button-face :inherit link)
   (lui-highlight-face :foreground base7 :weight bold)
   (lui-time-stamp-face :foreground magenta :weight bold)

   ;; magit 2.1
   (magit-bisect-bad :foreground base1 :background red :box (:color base6))
   (magit-bisect-good :foreground base2 :background green :box (:color base7))
   (magit-bisect-skip :foreground base2 :background yellow :box (:color base7))
   (magit-blame-heading :inherit header-line)
   (magit-branch-local :foreground base6)
   (magit-branch-remote :foreground green)
   (magit-cherry-equivalent :foreground magenta)
   (magit-cherry-unmatched :foreground cyan)
   (magit-diff-added :foreground green)
   (magit-diff-added-highlight :foreground green :weight bold)
   (magit-diff-base :foreground blue)
   (magit-diff-base-highlight :foreground blue :weight bold)
   (magit-diff-context :background base1)
   (magit-diff-context-highlight :background base1 :weight bold)
   (magit-diff-file-heading-selection :foreground orange :inherit magit-diff-file-heading-highlight)
   (magit-diff-hunk-heading :background base2)
   (magit-diff-hunk-heading-highlight :background base2 :weight bold)
   (magit-diff-hunk-heading-selection :foreground orange :inherit magit-diff-hunk-heading-highlight)
   (magit-diff-lines-heading :foreground base2 :background orange)
   (magit-diff-removed :foreground red)
   (magit-diff-removed-highlight :foreground red :weight bold)
   (magit-diffstat-added :foreground green)
   (magit-diffstat-removed :foreground red)
   (magit-dimmed :inherit shadow)
   (magit-hash :foreground base5)
   (magit-log-author :foreground orange)
   (magit-log-date :foreground magenta)
   (magit-log-graph :foreground base5)
   (magit-process-ng :foreground red)
   (magit-process-ok :foreground green)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-checkout :foreground base5)
   (magit-reflog-cherry-pick :foreground green)
   (magit-reflog-commit :foreground green)
   (magit-reflog-merge :foreground green)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :foreground red)
   (magit-refname :foreground violet)
   (magit-section-heading :foreground yellow :weight bold)
   (magit-section-heading-selection :foreground orange :weight bold)
   (magit-section-highlight :background base2)
   (magit-sequence-drop :foreground red)
   (magit-sequence-head :foreground base5)
   (magit-sequence-part :foreground yellow)
   (magit-sequence-stop :foreground green)
   (magit-signature-bad :foreground red)
   (magit-signature-good :foreground green)
   (magit-signature-untrusted :foreground cyan)
   (magit-tag :foreground yellow)

   ;; magit 1.4
   (magit-item-highlight :inherit highlight)
   (magit-log-head-label-bisect-bad :foreground base1 :background red :box (:color base6))
   (magit-log-head-label-bisect-good :foreground base2 :background green :box (:color base7))
   (magit-log-head-label-bisect-skip :foreground base2 :background yellow :box (:color base7))
   (magit-log-head-label-default :background base4)
   (magit-log-head-label-head :foreground yellow :background base1 :box t)
   (magit-log-head-label-local :background base1 :box (:color cyan))
   (magit-log-head-label-patches :foreground base2 :background orange :box (:color base6))
   (magit-log-head-label-remote :background base1 :box (:color magenta))
   (magit-log-head-label-tags :inherit magit-log-head-label-bisect-skip)
   (magit-log-head-label-wip :foreground base4 :background base1 :box t)
   (magit-log-reflog-label-checkout :background base1 :box t)
   (magit-log-reflog-label-cherry-pick :inherit magit-log-head-label-bisect-good)
   (magit-log-reflog-label-commit :foreground base2 :background base5 :box (:color base7))
   (magit-log-reflog-label-other :inherit magit-log-head-label-wip)
   (magit-log-reflog-label-rebase :foreground green :background base1 :box t)
   (magit-log-reflog-label-remote :background base1 :box t)
   (magit-log-reflog-label-reset :inherit magit-log-head-label-bisect-bad)
   (magit-log-sha1 :foreground orange)
   (git-rebase-hash :foreground orange)

   ;; markdown-mode
   (markdown-header-face-1 :background base2)
   (markdown-header-face-2 :background base3)
   (markdown-header-face-3 :background base4)
   (markdown-header-face-4 :background base2)
   (markdown-header-face-5 :background base3)
   (markdown-header-face-6 :background base4)

   ;; org-mode
   (org-agenda-dimmed-todo-face :inherit shadow)
   (org-agenda-done :foreground green)
   (org-agenda-restriction-lock :inherit highlight)
   (org-agenda-structure :foreground base5)
   (org-clock-overlay :inherit secondary-selection)
   (org-column :background base2)
   (org-column-title :inherit org-column :underline t)
   (org-date :foreground cyan :underline t)
   (org-date-selected :foreground base2 :background magenta)
   (org-document-info :foreground base5)
   (org-document-title :weight bold)
   (org-done :foreground green :weight bold)
   (org-drawer :foreground base5)
   (org-ellipsis :inherit shadow :weight bold)
   (org-footnote :foreground base5 :underline t)
   (org-formula :foreground orange)
   (org-habit-alert-face :background yellow)
   (org-habit-alert-future-face :background yellow)
   (org-habit-clear-face :background blue)
   (org-habit-clear-future-face :background blue)
   (org-habit-overdue-face :background red)
   (org-habit-overdue-future-face :background red)
   (org-habit-ready-face :background green)
   (org-habit-ready-future-face :background green)
   (org-headline-done :foreground yellow)
   (org-hide :background base0)
   (org-latex-and-related :foreground orange)
   (org-scheduled :foreground green)
   (org-scheduled-previously :foreground orange)
   (org-scheduled-today :inherit org-scheduled)
   (org-sexp-date :foreground cyan)
   (org-table :foreground violet)
   (org-time-grid :foreground yellow)
   (org-todo :foreground red :weight bold)
   (org-upcoming-deadline :foreground orange)

   ;; popup
   (popup-face :foreground base6 :background base2)
   (popup-isearch-match :foreground base7 :background base4)
   (popup-menu-mouse-face :foreground base7 :background base4)
   (popup-menu-selection-face :foreground base7 :background base4)
   (popup-scroll-bar-background-face :background base2)
   (popup-scroll-bar-foreground-face :background base4)
   (popup-summary-face :foreground base4 :background base2)
   (popup-tip-face :foreground base7 :background base4)

   ;; powerline
   (powerline-active1 :foreground base5 :background base2)
   (powerline-active2 :foreground base5 :background base3)
   (powerline-inactive1 :foreground base4 :background base1)
   (powerline-inactive2 :foreground base4 :background base2)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground base6)
   (rainbow-delimiters-depth-2-face :inherit outline-1)
   (rainbow-delimiters-depth-3-face :inherit outline-2)
   (rainbow-delimiters-depth-4-face :inherit outline-3)
   (rainbow-delimiters-depth-5-face :inherit outline-4)
   (rainbow-delimiters-depth-6-face :inherit outline-5)
   (rainbow-delimiters-depth-7-face :inherit outline-6)
   (rainbow-delimiters-depth-8-face :inherit outline-7)
   (rainbow-delimiters-depth-9-face :inherit outline-8)
   (rainbow-delimiters-unmatched-face :foreground base2 :background base6)

   ;; rst-mode
   (rst-level-1 :background base2)
   (rst-level-2 :background base3)
   (rst-level-3 :background base4)
   (rst-level-4 :background base2)
   (rst-level-5 :background base3)
   (rst-level-6 :background base4)

   ;; smartparens
   (sp-show-pair-match-face :inherit show-paren-match)
   (sp-show-pair-mismatch-face :inherit show-paren-mismatch)

   ;; smart-mode-line
   (sml/charging :foreground green)
   (sml/discharging :foreground cyan)
   (sml/filename :foreground cyan :weight normal)
   (sml/global :foreground base6)
   (sml/line-number :foreground base5)
   (sml/modes :foreground base4)
   (sml/modified :foreground green :weight bold)
   (sml/outside-modified :foreground base7 :background red)
   (sml/prefix :foreground orange)
   (sml/read-only :foreground yellow)

   ;; web-mode
   (web-mode-block-attr-name-face :foreground green)
   (web-mode-block-attr-value-face :foreground cyan)
   (web-mode-block-face :background base1)
   (web-mode-comment-keyword-face :weight bold)
   (web-mode-current-column-highlight-face :inherit highlight)
   (web-mode-current-element-highlight-face :inherit web-mode-block-face)
   (web-mode-doctype-face :foreground base5)
   (web-mode-error-face :inherit error)
   (web-mode-html-attr-name-face :foreground base5)
   (web-mode-html-tag-bracket-face :foreground magenta)
   (web-mode-html-tag-face :foreground violet)
   (web-mode-inlay-face :inherit web-mode-block-face)
   (web-mode-json-context-face :inherit web-mode-json-key-face)
   (web-mode-json-key-face :foreground magenta)
   (web-mode-param-name-face :foreground base5)
   (web-mode-symbol-face :foreground yellow)
   (web-mode-whitespace-face :inherit whitespace-space-after-tab)

   ;; undo-tree
   (undo-tree-visualizer-default-face :inherit shadow)
   (undo-tree-visualizer-current-face :foreground orange)
   (undo-tree-visualizer-active-branch-face :foreground unspecified :weight bold)
   (undo-tree-visualizer-register-face :foreground yellow)
   (undo-tree-visualizer-unmodified-face :foreground cyan)
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'cyan)

;;; cyan-theme.el ends here
