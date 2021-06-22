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
  firefox-ui-fix = {
    pname = "firefox-ui-fix";
    version = "904485b99eb66dcc88443a781a8b87c9cc364d93";
    src = fetchgit {
      url = "https://github.com/black7375/Firefox-UI-Fix";
      rev = "904485b99eb66dcc88443a781a8b87c9cc364d93";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "0jb7x0sa0zcgz3lilh3zxf92vc3dgvlqkbkjrpa2096q370apb30";
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
    version = "v4.31.1";
    src = fetchgit {
      url = "https://github.com/richardwilkes/gcs";
      rev = "v4.31.1";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "125caz8r5dp2gxpg1mjrwmyyrzqmc4vqx2zl3lkhrlllknnczqrf";
    };
    
  };
  ohmyzsh = {
    pname = "ohmyzsh";
    version = "e32d4b1e195f4c7777844beea97af42bd93434eb";
    src = fetchgit {
      url = "https://github.com/ohmyzsh/ohmyzsh";
      rev = "e32d4b1e195f4c7777844beea97af42bd93434eb";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "196pc8is2fpk9msi1dfzqa50xvh6ixbn665q284rn4w71424yhzx";
    };
    
  };
  stumpwm-contrib = {
    pname = "stumpwm-contrib";
    version = "3311c572f2b5b50c23623db4929e97525bca5443";
    src = fetchgit {
      url = "https://github.com/stumpwm/stumpwm-contrib";
      rev = "3311c572f2b5b50c23623db4929e97525bca5443";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "1xajwhsyvciw85r2yy762pygpqmhg40j4hyap2cq7fsl2p8d0igp";
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
