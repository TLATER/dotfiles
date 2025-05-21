((nil . ((compile-command . "nixos-rebuild build --flake .#")))
 (emacs-lisp-mode . ((fill-column . 88)
                     (indent-tabs-mode . nil)
                     (elisp-lint-indent-specs . ((use-package . 1)
                                                 (reformatter-define . 1)))))
 (rust-ts-mode . ((eglot-workspace-configuration
                   . (:rust-analyzer (:linkedProjects ["./pkgs/packages/desktopctl/Cargo.toml"]))))))
