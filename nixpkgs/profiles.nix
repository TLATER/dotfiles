{
  # Physical "PC"s, that sit in front of me.
  pcs = let
    pc-modules = [
      ./.
      ./configurations/graphical-programs
      ./configurations/graphical-programs/pcs.nix
      ./configurations/tty-programs
      ./configurations/tty-programs/pcs.nix
    ];
  in {
    personal = { ... }: {
      imports = pc-modules ++ [
        ./configurations/graphical-programs/games.nix
        ./configurations/tty-programs/mail/personal.nix
      ];
    };

    work = { ... }: {
      imports = pc-modules ++ [
        ./configurations/tty-programs/mail/work.nix
        ./configurations/tty-programs/work.nix
      ];
    };
  };

  # i.e. VMs, remote servers and such
  minimal = {
    text = { ... }: { imports = [ ./. ./configurations/tty-programs ]; };

    graphical = { ... }: {
      imports = [
        ./.
        ./configurations/tty-programs
        ./configurations/graphical-programs
      ];
    };
  };

}
