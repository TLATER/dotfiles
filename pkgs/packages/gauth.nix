{ sources, buildGoModule }:
buildGoModule rec {
  inherit (sources.gauth) pname version src;
  vendorHash = null;
  meta.mainProgram = "${pname}";
}
