{
  fetchFromGitHub,
  irrlicht,
  wayland,
  libxkbcommon,
}:
irrlicht.overrideAttrs (old: {
  inherit (old) version;

  src = fetchFromGitHub {
    owner = "edo9300";
    repo = "irrlicht1-8-4";
    rev = "ddad09ea5e1888df424d36071558afc10d0b4f61";
    hash = "sha256-sVW1JaizN4WyCrTOezCOLyFOT3nrqUqaJAVqTR9+DrI=";
  };

  buildInputs = old.buildInputs ++ [
    libxkbcommon
    wayland
  ];

  preBuild = ''
    makeFlagsArray+=(sharedlib NDEBUG=1)
  '';
})
