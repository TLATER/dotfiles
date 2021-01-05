{ pkgs }:

with pkgs;

buildGoModule rec {
  pname = "gauth";
  version = "c57414b83b103d71ab56ffe51b69d56b28a6578a";

  src = fetchFromGitHub {
    owner = "pcarrier";
    repo = "gauth";
    rev = "${version}";
    sha256 = "1ywn71280awx3hq26jnzssxmjs008sl4m92rj63xpmja52w0vkql";
  };

  vendorSha256 = "0mzfydmvv5z2aj4ni5zqdzh7j078gr7bnrz9p88lklk2banrgyyz";
}
