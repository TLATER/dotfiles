{
  sources,
  buildGoModule,
}:
buildGoModule rec {
  inherit (sources.gauth) pname version src;
  vendorSha256 = null;
  meta.mainProgram = "${pname}";
}
