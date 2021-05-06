# dotfiles

My NixOS-managed dotfiles repository.

## Installation

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
home-manager switch --flake github:tlater/dotfiles#tlater
```

#### Flake Targets

Possible targets are:

- `github:tlater/dotfiles#tlater`
  - A minimal configuration that is intended for text-only interfaces,
    what you'd want on a headless server.
- `github:tlater/dotfiles#graphical`
  - A slightly bigger set of options for things like graphical VMs.

### NixOS

This requires using the home-manager flake module. This flake provides
a utility function to make this as easy as possible
(`lib.nixosConfigurationFromProfile`), which will create a module from
a profile and username that can simply be passed to
`nixpkgs.lib.nixosSystem`.

Minimal example for a personal PC with the user tlater:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    home-manager.url = "github:nix-community/home-manager/release-20.09";
    dotfiles.url = "github:tlater/dotfiles";
  };

  outputs = { nixpkgs, home-manager, dotfiles, ... }: {
    nixosConfigurations = {
      example = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          home-manager.nixosModules.home-manager
          (dotfiles.lib.nixosConfigurationFromProfile
            dotfiles.profiles.pcs.personal "tlater")
        ];
      };
    };
  };
}
```
