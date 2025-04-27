{
  sources,
  irrlicht,
  wayland,
  libxkbcommon,
}:
irrlicht.overrideAttrs (old: {
  inherit (sources.edopro-irrlicht) pname version src;
  buildInputs = old.buildInputs ++ [
    libxkbcommon
    wayland
  ];

  preBuild = ''
    makeFlagsArray+=(sharedlib NDEBUG=1)
  '';
})
