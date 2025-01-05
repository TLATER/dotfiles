;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((compile-command . "nixos-rebuild build --flake .#")))
 (emacs-lisp-mode . ((fill-column . 88)
                     (indent-tabs-mode . nil)
                     (elisp-lint-indent-specs . ((use-package . 1)
                                                 (reformatter-define . 1)))))
 (rust-mode
  . ((eglot-workspace-configuration
      . (:rust-analyzer (:linkedProjects ["./home-config/dotfiles/eww/utils/Cargo.toml"]))))))
