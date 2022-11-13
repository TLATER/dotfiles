# dotfiles

My NixOS-managed dotfiles repository.

## Installation

### NixOS configurations

This flake contains two NixOS configurations, `yui` and
`ct-lt-02052`. Both come with the home-manager-managed dotfiles
pre-installed.

### Non-NixOS

For non-NixOS hosts, this requires nix >=2.4 with flakes enabled. To
install this on an arbitrary Linux distro, follow the [upstream nix
installation instructions](https://nixos.org/download.html) and then
run:

```bash
nix-env -iA nixpkgs.nixFlakes
mkdir -p ~/.config/nix
echo 'experimental-features = nix-command flakes' > ~/.config/nix/nix.conf
```

After that, enter a devshell for this flake and install it with
home-manager:

```bash
nix develop github:tlater/dotfiles
home-manager switch --flake github:tlater/dotfiles#gnome-vm
```
