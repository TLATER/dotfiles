{
  imports = [
    ./extensions.nix
    ./lepton.nix
  ];

  programs.librewolf = {
    enable = true;

    languagePacks = [
      "af"
      "de"
      "en-US"
      "nl"
      "zh-CH"
    ];
  };

  home.file.".librewolf/librewolf.overrides.cfg".source = ./librewolf.overrides.js;
}
