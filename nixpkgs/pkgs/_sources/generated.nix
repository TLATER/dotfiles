# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl }:
{
  bauer = {
    pname = "bauer";
    version = "v1.5.3";
    src = fetchgit {
      url = "https://github.com/matthewbauer/bauer";
      rev = "v1.5.3";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "14k1nzjrgl8ppm785v32h4bi11248y194mjpblqym5hjc3nxf710";
    };
    
  };
  elpa-spinner = {
    pname = "elpa-spinner";
    version = "1.7.3";
    src = fetchurl {
      sha256 = "188i2r7ixva78qd99ksyh3jagnijpvzzjvvx37n57x8nkp8jc4i4";
      url = "https://elpa.gnu.org/packages/spinner-1.7.3.el.lz";
    };
  };
  firefox-ui-fix = {
    pname = "firefox-ui-fix";
    version = "1c150b243242268f3c54b82a16bd33ba12f02dbb";
    src = fetchgit {
      url = "https://github.com/black7375/Firefox-UI-Fix";
      rev = "1c150b243242268f3c54b82a16bd33ba12f02dbb";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "0ih99j69jims6hkrrahhbyb8vysjcamhlj2qn1gmwznlbdapbaf2";
    };
  };
  gauth = {
    pname = "gauth";
    version = "v1.1";
    src = fetchgit {
      url = "https://github.com/pcarrier/gauth";
      rev = "v1.1";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "179jxp2n6bms0x5brqyhd1437d5gfh75jn3sl0m7d04jf3kk3bi1";
    };
    
  };
  gcs = {
    pname = "gcs";
    version = "v4.33.0";
    src = fetchgit {
      url = "https://github.com/richardwilkes/gcs";
      rev = "v4.33.0";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "0i36h08kda861l210qrfh4xlyzidkxcl88rmz6q09v10g354jgdi";
    };
  };
  ohmyzsh = {
    pname = "ohmyzsh";
    version = "d9ad99531f74df8b0d6622feeab5e253528b43d0";
    src = fetchgit {
      url = "https://github.com/ohmyzsh/ohmyzsh";
      rev = "d9ad99531f74df8b0d6622feeab5e253528b43d0";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "192wpifg8ylq149hy39qmwfks5jfp9zdyn43pdjnpf52fqqs42zm";
    };
  };
  stumpwm-contrib = {
    pname = "stumpwm-contrib";
    version = "6c72e5bdf5fd716542eab953cbaa9a1ed0fedcec";
    src = fetchgit {
      url = "https://github.com/stumpwm/stumpwm-contrib";
      rev = "6c72e5bdf5fd716542eab953cbaa9a1ed0fedcec";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "0ahk4irf5mfp7mf712mdbyh6dx4ip9hh5vz7m3zdfk3byvd38j59";
    };
  };
  tridactyl-emacs = {
    pname = "tridactyl-emacs";
    version = "abc05ae33d282dc8fb999f0706efe406766ed9dc";
    src = fetchgit {
      url = "https://github.com/jumper047/tridactyl_emacs_config";
      rev = "abc05ae33d282dc8fb999f0706efe406766ed9dc";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "0gmp55kbdrhmdfdzij07jpblqgw6badfnsjgqxfrkhwdwkcgzm5r";
    };
  };
  zsh-background-notify = {
    pname = "zsh-background-notify";
    version = "d5f0430cb052f82c433c17707816910da87e201e";
    src = fetchgit {
      url = "https://github.com/t413/zsh-background-notify";
      rev = "d5f0430cb052f82c433c17707816910da87e201e";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "0p8fk50bxr8kg2v72afg7f2n09n9ap0yn7gz1i78nd54l0wc041n";
    };
    
  };
}
