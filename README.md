# dotfiles

My NixOS-managed dotfiles repository. For the time being this cannot
be deployed easily without a downstream NixOS configuration (see my
[nixos-hosts](https://github.com/TLATER/nixos-hosts) for an example).

It's possible in theory with the help of `nix build`, but I have not
bothered to implement it for now.

A more reasonable deployment method depends on the resolution of
[nix-community/home-manager#1783](https://github.com/nix-community/home-manager/issues/1783).

I attempt to keep things in traditional dotfiles where it's simple
enough, so feel free to rummage through the `dotfiles` directory.
