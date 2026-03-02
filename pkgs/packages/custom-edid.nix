{
  runCommand,
  xorg,
  edid-generator,
}:
edid-generator.overrideAttrs (old: {
  # passAsFile = [ ];
  clean = true;

  # modelines = ''Modeline "3840x2160" 712.34 3840 4152 4576 5312 2160 2161 2164 2235 -HSync +Vsync'';

  modelinesPath =
    (runCommand "modelines" { nativeBuildInputs = [ xorg.xorgserver ]; } ''
      gtf 3840 2160 120 > modeline
      sed -i 's/"3840x2160_120.00"/"VTCL65C825"/' modeline
      grep -v '# ' modeline > $out
    '').outPath;
})
