# dotfiles

My NixOS-managed dotfiles repository.

## Installation

### NixOS configurations

#### Reinstalling an existing configuration

These can be installed using
[nixos-anywhere](https://github.com/numtide/nixos-anywhere/), however
the installation steps are subtly different from the upstream ones:

> **Warning**
> yui has not yet been set up for this process

1. Get the gpg secret directory from a backup. If no backup is
   available, set up a new gpg directory as in the new machine
   instructions.
2. Boot the NixOS install disk on the machine in question, set up
   networking and a user password.
3. ssh into the machine, copy the ssh public key into
   `/root/.ssh/authorized_keys`
4. Place the desired disk encryption password in a file
   (`secret.key`), ensuring it does *not* end in a newline (`echo -e`).
5. Run `nix run --inputs-from . nixos-anywhere -- -f .#<hostname> root@<ip> --no-reboot --disk-encryption-keys /tmp/secret.key secret.key`
   - If some permission error around `secret.key` pops up, rerun the
     command without the `--disk-encryption-keys`. They will already
     have been copied over, some kind of upstream bug.
6. Before rebooting:
   1. Copy the gpg dir acquired in step 1 to `/mnt/var/lib/sops`
   2. Place a copy of `/etc/sops/secrets.yaml` in `/mnt/etc/sops/secrets.yaml`.
   3. Set a password for tlater with `nixos-enter` and `passwd`

#### Setting up a new machine

Before the configuration can be installed, we need to set up a few
basic things.

It's easiest to do this when the system in question is already
running, and we're ssh'd into it, so we can read out some device
data. So boot into the NixOS installer first (will be useful later
anyway).

Create a new subdirectory of `nixos-config` named after the new host,
and create a new `nixosConfigurations` entry for it.

Then set up:

1. Basic networking. Typically a bond config, see ren/yui for
   examples.
2. Make sure to include one of the network modules so that wireless
   networking is set up on first boot.
3. Generate and include a new `hardware-configuration.nix`:
    ```console
    nixos-generate-config --no-filesystems --show-hardware-config
    ```
4. Set up a new sops target.
   1. Firstly, create a new gpg dir according to [these
      instructions](https://github.com/Mic92/sops-nix#use-with-gpg-instead-of-ssh-keys).
   2. Then, add the generated fingerprint to `.sops.yaml`, and
      re-encrypt `/etc/sops/secrets.yaml`.

##### GPG/sops setup

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
