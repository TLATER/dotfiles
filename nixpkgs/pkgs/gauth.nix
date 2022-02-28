{ sources, buildGoModule }:

buildGoModule rec {
  inherit (sources.gauth) pname version src;
  vendorSha256 = "sha256-S0a8V7lgU1AvEKaI6sc1ZEC+acj650p7npl0ugByRSQ=";
  meta.mainProgram = "${pname}";
}
