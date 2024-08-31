# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  bauer = {
    pname = "bauer";
    version = "v2.0.1";
    src = fetchFromGitHub {
      owner = "matthewbauer";
      repo = "bauer";
      rev = "v2.0.1";
      fetchSubmodules = false;
      sha256 = "sha256-jenFLJeVX1OlckvCv+c/u2GG8UNQatfQ8OwPu1cs4Q8=";
    };
  };
  deepfilternet = {
    pname = "deepfilternet";
    version = "v0.5.6";
    src = fetchFromGitHub {
      owner = "Rikorose";
      repo = "DeepFilterNet";
      rev = "v0.5.6";
      fetchSubmodules = false;
      sha256 = "sha256-5bYbfO1kmduNm9YV5niaaPvRIDRmPt4QOX7eKpK+sWY=";
    };
  };
  drivestrike = {
    pname = "drivestrike";
    version = "2.1.22-31";
    src = fetchurl {
      url = "https://app.drivestrike.com/static/yum/drivestrike.rpm";
      sha256 = "sha256-2O0TjRhuwLd+QPUxV9tHeuWYtGoRnBa6icU7DMmxWyI=";
    };
  };
  edopro = {
    pname = "edopro";
    version = "29216bd54ec0ad72d93e527cda0adb5763442140";
    src = fetchFromGitHub {
      owner = "edo9300";
      repo = "edopro";
      rev = "29216bd54ec0ad72d93e527cda0adb5763442140";
      fetchSubmodules = true;
      sha256 = "sha256-13mBGtbtEWQAf+sgmoK54wY+XpOtuB/Y+8ZqmvQ+2Ps=";
    };
    date = "2024-08-21";
  };
  edopro-assets = {
    pname = "edopro-assets";
    version = "40.1.4";
    src = fetchTarball {
      url = "https://github.com/ProjectIgnis/edopro-assets/releases/download/40.1.4/ProjectIgnis-EDOPro-40.1.4-linux.tar.gz";
      sha256 = "sha256-vZhkWJ1ZoNEwdc5kM1S0hyXnWmupiTOanCi9DCuqw/k=";
    };
  };
  edopro-irrlicht = {
    pname = "edopro-irrlicht";
    version = "226e9eab21f48ad91b032fe4a32ec0b4899a3130";
    src = fetchFromGitHub {
      owner = "edo9300";
      repo = "irrlicht1-8-4";
      rev = "226e9eab21f48ad91b032fe4a32ec0b4899a3130";
      fetchSubmodules = false;
      sha256 = "sha256-Em3ArvjAH4g67RaINqjSPg6f2r/M5UVgX+MHLxAWADE=";
    };
    date = "2024-08-21";
  };
  eglot-x = {
    pname = "eglot-x";
    version = "ee39a0edd43431eee899fc7787d936731c157689";
    src = fetchFromGitHub {
      owner = "nemethf";
      repo = "eglot-x";
      rev = "ee39a0edd43431eee899fc7787d936731c157689";
      fetchSubmodules = false;
      sha256 = "sha256-eu9qq8fDtri6i5Cr9qDOdchiQ133+UjpKUFlNnANmp8=";
    };
    date = "2024-08-19";
  };
  firefox-ui-fix = {
    pname = "firefox-ui-fix";
    version = "v8.6.2";
    src = fetchFromGitHub {
      owner = "black7375";
      repo = "Firefox-UI-Fix";
      rev = "v8.6.2";
      fetchSubmodules = false;
      sha256 = "sha256-19aivAOvXeFOnsP5XhitK3+xk1lwUyXoTK8BE84qbFQ=";
    };
  };
  gauth = {
    pname = "gauth";
    version = "v1.3.0";
    src = fetchFromGitHub {
      owner = "pcarrier";
      repo = "gauth";
      rev = "v1.3.0";
      fetchSubmodules = false;
      sha256 = "sha256-GU6HKha7Y01HJX6pyYHORUkFKgl9mWtDd65d+3pYxjI=";
    };
  };
  gcs = {
    pname = "gcs";
    version = "v5.27.0";
    src = fetchFromGitHub {
      owner = "richardwilkes";
      repo = "gcs";
      rev = "v5.27.0";
      fetchSubmodules = false;
      sha256 = "sha256-N4a+7a1XpDXsZTDiTh1AZIzXfk/A+gV4uBZdGZh+S5I=";
    };
  };
  ohmyzsh = {
    pname = "ohmyzsh";
    version = "c68ff8aeedc2b779ae42d745457ecd443e22e212";
    src = fetchFromGitHub {
      owner = "ohmyzsh";
      repo = "ohmyzsh";
      rev = "c68ff8aeedc2b779ae42d745457ecd443e22e212";
      fetchSubmodules = false;
      sha256 = "sha256-RGomSLwXoQDFkq1kFWfFMRdMmpW4dlLQ4JR5EDXW3/8=";
    };
    date = "2024-08-30";
  };
  phosphor-icons = {
    pname = "phosphor-icons";
    version = "9634e6f3bccabbc1bf2c59dc283abd283d00946d";
    src = fetchFromGitHub {
      owner = "phosphor-icons";
      repo = "web";
      rev = "9634e6f3bccabbc1bf2c59dc283abd283d00946d";
      fetchSubmodules = false;
      sha256 = "sha256-Ul0UtnsrJ4pUY+rozU7W6DIpIq7DstQN69sOql4x6Yc=";
    };
    date = "2024-05-02";
  };
  tridactyl-emacs = {
    pname = "tridactyl-emacs";
    version = "ee82f3de875e7cda518cfd2c469ec21ed94c3cfb";
    src = fetchFromGitHub {
      owner = "jumper047";
      repo = "tridactyl_emacs_config";
      rev = "ee82f3de875e7cda518cfd2c469ec21ed94c3cfb";
      fetchSubmodules = false;
      sha256 = "sha256-OGSVKUoZ9E5yqJQfTnfx6QW4vLbdQ5/V3VX65TuQG4k=";
    };
    date = "2023-12-22";
  };
}
