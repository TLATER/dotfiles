{
  nixpkgs,
  home-manager,
  overlays,
}: rec {
  # Create a module with correctly set overlays from a given
  # profile.
  #
  nixosModuleFromProfile = profile: {...} @ args:
    (profile args)
    // {
      nixpkgs.overlays = overlays;
    };

  # Create a NixOS module that configures home-manager to use
  # the given profile.
  #
  nixosConfigurationFromProfile = profile: username: {...} @ args: {
    home-manager.users.${username} = nixosModuleFromProfile profile;
  };

  # Create a homeManagerConfiguration that can be installed
  # using `home-manager --flake`.
  #
  homeConfigurationFromProfile = profile: {
    system,
    username ? "tlater",
    homeDirectory ? "/home/${username}",
  }:
    home-manager.lib.homeManagerConfiguration {
      inherit homeDirectory system username;
      configuration = nixosModuleFromProfile profile;
    };

  withPkgs = system: f: let
    pkgs = import nixpkgs {inherit overlays system;};
  in
    f pkgs;

  localPackagesExcept = system: exceptions:
    withPkgs system (pkgs:
      removeAttrs (import ../pkgs {inherit pkgs;}) exceptions);
}
