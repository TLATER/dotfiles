;;; programming-languages.el --- Configuration for specific programming languages      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tristan Daniël Maat

;; Author: Tristan Daniël Maat <tm@tlater.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-and-compile
  (require 'leaf))

;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------
;;; Language mode configuration
;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------

;; ----------------------------------------------------------------------------------
;;; Bazel
;; ----------------------------------------------------------------------------------

(leaf bazel
  :ensure t
  :defvar package-selected-packages package-archive-contents
  :mode `((,(rx ".bzl" string-end) . bazel-starlark-mode)
         (,(rx (or "BUILD" "BUILD.bazel") string-end) . bazel-build-mode))
  :commands bazel-buildifier)

;; ----------------------------------------------------------------------------------
;;; C
;; ----------------------------------------------------------------------------------

(leaf cc-mode
  :require eglot
  :hook (c-mode-common-hook . eglot-ensure))

;; ----------------------------------------------------------------------------------
;;; CSV
;; ----------------------------------------------------------------------------------

(leaf csv-mode
  :ensure t
  :mode `(,(rx ".csv" string-end)))

;; ----------------------------------------------------------------------------------
;;; Dockerfile
;; ----------------------------------------------------------------------------------

(leaf dockerfile-ts-mode
  :mode `(,(rx (or
                (and "Dockerfile" (opt "." (1+ anything)))
                (and "." (char "Dd") "ockerfile"))
               string-end))
  :hook (dockerfile-ts-mode-hook . (lambda () (setq-local devdocs-current-docs '("docker")))))

;; ----------------------------------------------------------------------------------
;;; GLSL
;; ----------------------------------------------------------------------------------

(leaf glsl-mode
  :ensure t
  :mode `(,(rx (or ".glsl" ".vert" ".frag" ".geom") string-end)))

;; ----------------------------------------------------------------------------------
;;; Gnuplot
;; ----------------------------------------------------------------------------------

(leaf gnuplot
  :ensure t
  :mode `(,(rx (or ".p" ".gp" ".gnuplot") string-end) . gnuplot-mode)
  :hook (gnuplot-mode-hook . (lambda () (setq-local devdocs-current-docs '("gnuplot")))))

;; ----------------------------------------------------------------------------------
;;; GraphQL
;; ----------------------------------------------------------------------------------

(leaf graphql-mode
  :ensure t
  :mode `(,(rx (or ".graphql" ".gql") string-end)))

;; ----------------------------------------------------------------------------------
;;; Haskell
;; ----------------------------------------------------------------------------------

(leaf haskell-mode
  :ensure t
  :commands haskell-mode-stylish-buffer
  :mode `(,(rx ".hs" string-end))
  :hook (haskell-mode-hook . interactive-haskell-mode)
  :hook (haskell-mode-hook . (lambda () (setq-local devdocs-current-docs '("haskell~9"))))
  :bind (:haskell-mode-map
         ("C-c `" . haskell-interactive-bring)))

;; ----------------------------------------------------------------------------------
;;; JSON & co.
;; ----------------------------------------------------------------------------------

(leaf json-ts-mode
  :require eglot
  :mode `(,(rx (or ".json" ".jsonld") string-end))
  :magic-fallback ("^[{[]$" . json-ts-mode)
  :hook (json-ts-mode-hook . eglot-ensure)
  :defvar eglot-server-programs
  :defer-config
  (add-to-list 'eglot-server-programs
               '(json-ts-mode . ("biome" "lsp-proxy"))))

(leaf jsonnet-mode
  :ensure t
  :mode `(,(rx ".jsonnet" string-end)))

(leaf yaml-ts-mode
  :require eglot
  :mode `(,(rx (or ".yaml" ".yml" ".bst" "project.conf") string-end))
  :hook (yaml-ts-mode-hook . eglot-ensure)
  :hook (yaml-ts-mode-hook . (lambda () (setq-local tab-width 2))))

;; ----------------------------------------------------------------------------------
;;; KDL
;; ----------------------------------------------------------------------------------

(leaf kdl-mode
  :ensure t
  :mode `(,(rx ".kdl" string-end)))

;; ----------------------------------------------------------------------------------
;;; Kotlin
;; ----------------------------------------------------------------------------------

(leaf kotlin-mode
  :ensure t
  :require eglot
  :hook
  (kotlin-mode-hook . (lambda () (setq-local devdocs-current-docs '("kotlin~1.9"))))
  (kotlin-mode-hook . eglot-ensure)
  :mode `(,(rx ".kt" (optional "s") string-end)))

;; ----------------------------------------------------------------------------------
;;; Markdown
;; ----------------------------------------------------------------------------------

(leaf markdown-mode
  :ensure t
  :hook (markdown-mode-hook . (lambda () (setq-local devdocs-current-docs '("markdown"))))
  :mode `(,(rx (or
             (and (or ".md" ".mdwn") string-end)
             (and string-start "/tmp/neomutt-"))))
  :custom
  (markdown-command . '("pandoc" "--from=markdown" "--to=html5")))

;; ----------------------------------------------------------------------------------
;;; Mermaid
;; ----------------------------------------------------------------------------------

(leaf mermaid-mode
  :ensure t
  :mode `(,(rx ".mermaid" string-end)))

;; ----------------------------------------------------------------------------------
;;; nginx config files
;; ----------------------------------------------------------------------------------

(leaf nginx-mode
  :ensure t
  :mode `(,(rx "nginx.conf" string-end))
  :hook (markdown-mode-hook . (lambda () (setq-local devdocs-current-docs '("nginx")))))

;; ----------------------------------------------------------------------------------
;;; Nix
;; ----------------------------------------------------------------------------------

(leaf nix-ts-mode
  :ensure t
  :require eglot
  :hook
  (nix-ts-mode-hook . (lambda () (setq-local devdocs-current-docs '("nix"))))
  (nix-ts-mode-hook . eglot-ensure)
  :defer-config
  (add-to-list 'eglot-server-programs
               '(nix-ts-mode . ("nil"))))

;; ----------------------------------------------------------------------------------
;;; org
;; ----------------------------------------------------------------------------------

(leaf org
  :mode `(,(rx ".org" string-end) . org-mode)
  :custom
  (org-latex-listings . t)
  :defer-config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t))))

;; ----------------------------------------------------------------------------------
;;; Protobuf
;; ----------------------------------------------------------------------------------

(leaf protobuf-mode
  :ensure t
  :mode `(,(rx ".proto" string-end)))

;; ----------------------------------------------------------------------------------
;;; Python
;; ----------------------------------------------------------------------------------

(defun find-python-hook ()
  "Find and set the python executable."
  (setq-local python-shell-interpreter
              (cond
               ((executable-find "ipython") "ipython")
               ((executable-find "ipython3") "ipython3")
               ((executable-find "python3") "python3"))))

(leaf python
  :require eglot
  :mode `(,(rx ".py" string-end) . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook
  (python-ts-mode-hook . (lambda () (setq-local devdocs-current-docs '("python~3.11"))))
  (python-ts-mode-hook . eglot-ensure)
  (python-ts-mode-hook . find-python-hook)
  :custom
  (python-shell-interpreter-args . "--simple-prompt -i")
  :defer-config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("ruff" "server"))))

(leaf cython-mode
  :ensure t
  :mode `(,(rx (or ".pyx" ".pxd" ".pxi") string-end))
  :hook (cython-mode-hook . (lambda () (setq-local devdocs-current-docs '("python~3.11")))))

;; ----------------------------------------------------------------------------------
;;; Rust
;; ----------------------------------------------------------------------------------

(leaf rust-ts-mode
  :mode `(,(rx ".rs" string-end))
  :require eglot
  :hook
  (rust-ts-mode-hook . (lambda () (setq-local devdocs-current-docs '("rust"))))
  (rust-ts-mode-hook . eglot-ensure)
  (rust-ts-mode-hook . (lambda () (remove-hook 'flymake-diagnostic-functions #'rust-ts-flymake t)))
  :defer-config
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"
                                 :initializationOptions
                                 (:check
                                  (:command "clippy"))))))

;; Use rust-mode just for the compilation regexes
(leaf rust-mode
  :ensure t
  :after rust-ts-mode
  :require rust-compile)

;; ----------------------------------------------------------------------------------
;;; SCSS
;; ----------------------------------------------------------------------------------

(leaf scss-mode
  :mode `(,(rx (or ".sass" ".scss") string-end))
  :hook (scss-mode-hook . (lambda () (setq-local devdocs-current-docs '("css" "sass")))))

;; ----------------------------------------------------------------------------------
;;; *sh
;; ----------------------------------------------------------------------------------

(leaf sh-script
  :mode (`,(rx (or ".sh" ".bash" ".zsh")))
  :hook
  ((bash-ts-mode-hook sh-mode-hook) . eglot-ensure)
  ((bash-ts-mode-hook sh-mode-hook) . (lambda () (setq-local devdocs-current-docs '("bash")))))

(leaf nushell-ts-mode
  :ensure t
  :require eglot
  :mode `(,(rx ".nu" string-end))
  :hook
  (nushell-ts-mode-hook . eglot-ensure)
  :defer-config
  (add-to-list 'eglot-server-programs
               '(nushell-ts-mode . ("nu" "--lsp"))))

;; ----------------------------------------------------------------------------------
;;; Systemd
;; ----------------------------------------------------------------------------------

(leaf systemd
  :ensure t
  :mode `(,(rx (or".service" ".socket" ".device" ".mount" ".automount"
                ".swap" ".target" ".path" ".timer" ".slice" ".scope")
             string-end)
         . systemd-mode))

;; ----------------------------------------------------------------------------------
;;; Dart
;; ----------------------------------------------------------------------------------

(leaf dart-mode
  :ensure t
  :require eglot
  :mode `(,(rx ".dart" string-end))
  :hook
  (dart-mode-hook . (lambda () (set (make-local-variable 'eglot-x-client-commands) '())))
  (dart-mode-hook . eglot-ensure))

;; ----------------------------------------------------------------------------------
;;; Terraform
;; ----------------------------------------------------------------------------------

(leaf terraform-mode
  :ensure t
  :mode `(,(rx ".tf" string-end))
  :custom
  (terraform-format-on-save . t))

;; ----------------------------------------------------------------------------------
;;; TypeScript (and other web-related DSLs)
;; ----------------------------------------------------------------------------------

(leaf typescript-ts-mode
  :require eglot
  :mode `(,(rx ".ts" (? "x") string-end))
  :defer-config
  (add-to-list 'eglot-server-programs
               `((typescript-ts-mode :language-id "typescriptreact")
                 . ("typescript-language-server" "--stdio"))))

(leaf web-mode
  :ensure t
  :require eglot
  :mode `(,(rx (or ".pug" ".hbs") string-end))
  :hook
  (web-mode-hook . (lambda () (setq-local devdocs-current-docs
                                          '("html"
                                            "typescript"
                                            "css"
                                            "javascript"
                                            "dom"))))
  (web-mode-hook . eglot-ensure)
  :defer-config
  (add-to-list 'eglot-server-programs
               `(web-mode . ("biome" "lsp-proxy"))))

;; ----------------------------------------------------------------------------------
;;; yuck - widget markup for eww
;; ----------------------------------------------------------------------------------

(leaf yuck-mode
  :ensure t
  :mode `(,(rx ".yuck" string-end)))

(leaf zen-mode
  :ensure t
  :mode `(,(rx ".zs")))

;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------
;;; More generic programming-language support features
;; ----------------------------------------------------------------------------------
;; ----------------------------------------------------------------------------------

(leaf fancy-edit-indirect
  :load-path* "modules")

(leaf polymode
  :ensure t
  :require (nix-ts-mode fancy-edit-indirect)
  :mode `(,(rx ".nix" string-end) . poly-nix-mode)
  :bind (:polymode-mode-map
         ("C-c ;" . #'fancy-edit-indirect))
  :custom
  (polymode-mode-name-aliases .
   '((bash . bash-ts-mode)
     (md . markdown-mode)
     (markdown . markdown-mode)
     (nu . nushell-ts-mode)
     (python . python-ts-mode)))
  :config
  (pm-around-advice '(eglot-ensure flymake-mode) #'polymode-inhibit-in-indirect-buffers)
  (define-hostmode poly-nix-hostmode
    :mode 'nix-ts-mode)
  (define-auto-innermode poly-nix-string-innermode
    :head-matcher "/\\*[[:space:]]+[[:graph:]]+[[:space:]]+\\*/[[:space:]]+''"
    :tail-matcher (cons "[^']\\(?1:''\\)[^']" 1)
    :mode-matcher (cons "/\\*[[:space:]]+\\(?1:[[:graph:]]+\\)[[:space:]]+\\*/" 1)
    :body-indent-offset nix-ts-mode-indent-offset
    ;; :head-matcher (rx "/*" (one-or-more space) (one-or-more graphic) (one-or-more space) "*/" (one-or-more space) "''")
    ;; :tail-matcher (rx (not "'") "''" (not "'"))
    ;; :mode-matcher (rx "/*" (one-or-more space) (group-n 1 (one-or-more graphic)) (one-or-more space) "*/")
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-nix-mode
    :hostmode 'poly-nix-hostmode
    :innermodes '(poly-nix-string-innermode)))

;; ----------------------------------------------------------------------------------
;;; Language servers
;; ----------------------------------------------------------------------------------

(leaf company
  :ensure t
  :hook (after-init-hook . global-company-mode)
  :custom
  (company-idle-delay . 0.1))

(leaf eldoc
  :bind (("C-c l d" . 'eldoc-doc-buffer))
  :custom
  (eldoc-echo-area-prefer-doc-buffer . t)
  (eldoc-echo-area-use-multiline-p . .15)
  (eldoc-echo-area-display-truncation-message . 'nil)
  :config
  (global-eldoc-mode 1))

(leaf xref
  :bind (("C-c l f" . xref-find-definitions-other-window)))

(defun set-eldoc-compose ()
  "Set documentation strategy to compose to work around flymake interference."
  ;; TODO(tlater): Find a nicer way to implement
  (with-suppressed-warnings ((obsolete eldoc-documentation-strategy))
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)))

(leaf eglot
  :commands (eglot eglot-format eglot-managed-p)
  :hook (eglot-managed-mode-hook . set-eldoc-compose)
  :bind (:eglot-mode-map
         ("C-c l r" . eglot-rename)
         ("C-c l a" . eglot-code-actions)
         ("C-c l i" . consult-eglot-symbols))
  :defun eglot-alternatives
  :defvar eglot-workspace-configuration
  :setq-default (eglot-workspace-configuration .
    '((nil
       (formatting
        (command . ["nixfmt" "--strict"]))
       (nix (flake (autoEvalInputs . :json-false))))
      (pylsp
       (plugins
        (pydocstyle
         (enabled . t)
         ;; Does not currently work:
         ;; https://github.com/python-lsp/python-lsp-server/issues/159
         (match . ".*"))
        (pycodestyle
         ;; Make compatible with black:
         ;; https://black.readthedocs.io/en/stable/the_black_code_style/current_style.html#line-length
         (maxLineLength . 88)
         ;; Only E203 is incompatible with black and enabled by
         ;; default, but defining *any* ignore list will override
         ;; the default ignore list, so we need to recreate the
         ;; original.
         ;;
         ;; See also the small caveat under the huge table here:
         ;; https://pycodestyle.pycqa.org/en/latest/intro.html#error-codes
         (ignore . ["E203" "E121" "E123" "E126" "E133" "E226"
                    "E241" "E242" "E704" "W503" "W504" "W505"])))))))

(leaf eglot-x
  :ensure t
  :after eglot
  :require t
  :defun eglot-x-setup
  :config
  (eglot-x-setup))

(leaf flymake
  :hook (prog-mode-hook . flymake-mode)
  :bind (:flymake-mode-map
              ("C-c l e" . flymake-show-buffer-diagnostics)))

;; ----------------------------------------------------------------------------------
;;; Autoformatting
;; ----------------------------------------------------------------------------------

(leaf reformatter
  :ensure t
  :commands (biome-format-region
             biome-format-buffer
             clang-format-region
             clang-format-buffer
             latexindent-region
             latexindent-buffer
             topiary-nushell-region
             topiary-nushell-buffer
             topiary-sieve-region
             topiary-sieve-buffer)
  :defer-config
  ;; Work around `make-variable-buffer-local' being called at a
  ;; non-top-level.
  ;;
  ;; Note: Do not do this without first checking if no other warnings
  ;; are thrown. In theory, we could use `with-suppressed-warnings',
  ;; but it doesn't look like there's any documentation on how to
  ;; disable this particular warning, and grepping the emacs source
  ;; for `make-variable-buffer-local' didn't yield anything useful.
  (with-no-warnings
    (reformatter-define biome-format
      :program "biome"
      :args (list "format" (concat "--stdin-file-path=" buffer-file-name)))
    (reformatter-define clang-format
      :program "clang-format"
      :group 'glsl-mode
      :lighter " CF")
    (reformatter-define latexindent
      :program "latexindent"
      :group 'latex-mode
      :lighter " LF")
    (reformatter-define dart-format
      :program "dart"
      :args '("format"))
    (reformatter-define topiary-nushell
      :program "topiary"
      :group 'nushell-ts-mode
      :args '("format" "--language" "nu"))
    (reformatter-define topiary-sieve
      :program "topiary"
      :group 'sieve-mode
      :args '("format" "--language" "sieve"))))

(defun autoformat ()
  "Autoformat the current buffer."
  (interactive)
  (pcase major-mode
    ('glsl-mode
     (clang-format-buffer))
    ('latex-mode
     (latexindent-buffer))
    ('dart-mode
     (dart-format-buffer))
    ('nushell-ts-mode
     (topiary-nushell-buffer))
    ('sieve-mode
     (topiary-sieve-buffer))
    ((or 'mhtml-mode 'web-mode 'scss-mode 'json-mode)
     (biome-format-buffer))
    ('haskell-mode (haskell-mode-stylish-buffer))
    ((or 'bazel-mode
         (app (lambda (m) (get m 'derived-mode-parent)) 'bazel-mode))
     (bazel-buildifier))
    (_ (if (and (eglot-managed-p) (eglot-server-capable :documentFormattingProvider))
           (eglot-format)
         (message "No formatter for this file type")))))

(define-key global-map (kbd "C-c f") 'autoformat)

;; ----------------------------------------------------------------------------------
;;; Documentation
;; ----------------------------------------------------------------------------------

(defun open-docs ()
  "Open docs at point."
  (interactive)

  (let ((searchterm (symbol-name (symbol-at-point))))
    (pcase major-mode
      ('emacs-lisp-mode
       (describe-symbol (symbol-at-point)))
      (mode
       (cond ((and (eglot-managed-p) (eglot-server-capable :experimental :externalDocs))
              (cl-letf (((symbol-function 'browse-url) #'eww-browse-url))
                (eglot-x-open-external-documentation)))
             (devdocs-current-docs (devdocs-lookup nil searchterm))
             (t
              (eww-browse-url (concat "https://duckduckgo.com/lite/?q=%21"
                                      (string-trim-right (symbol-name mode) (rx (opt "-ts") "-mode" string-end))
                                      "%20"
                                      searchterm))))))))

(define-key global-map (kbd "C-h D") 'open-docs)

;;; programming-languages.el ends here
