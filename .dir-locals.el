((nil . ((compile-command . "nixos-rebuild build --flake .#")))
 (rust-mode
  . ((eglot-workspace-configuration
      . (:rust-analyzer
         (:check (:command "clippy")
          :linkedProjects ["./home-config/dotfiles/eww/desktop-logic/Cargo.toml"])))))
 (emacs-lisp-mode . ((fill-column . 88)
                     (indent-tabs-mode . nil)
                     (elisp-lint-indent-specs . ((use-package . 1)
                                                 (reformatter-define . 1))))))
