{ sources, buildGoModule }:

buildGoModule rec {
  inherit (sources.gauth) pname version src;
  vendorSha256 = "0mzfydmvv5z2aj4ni5zqdzh7j078gr7bnrz9p88lklk2banrgyyz";
}
