# dotfiles

My dotfiles repository. Deployment is managed through [Home
Manager](https://github.com/rycee/home-manager/) - simply symlink the
nixpkgs directory to `$XDG_CONFIG_HOME/nixpkgs/` and run `home-manager
switch`.

### Todo
- [ ] De-vendor zsh plugins
- [ ] Add gtk theming
- [ ] Figure out how to pre-compile stumpwm module FASL files
- [ ] Look into https://github.com/HugoReeves/nix-home for inspiration
      for a slightly more modular approach; might make changes between
      different machines easier to maintain.
