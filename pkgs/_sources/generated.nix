# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  bauer = {
    pname = "bauer";
    version = "v1.5.3";
    src = fetchFromGitHub {
      owner = "matthewbauer";
      repo = "bauer";
      rev = "v1.5.3";
      fetchSubmodules = false;
      sha256 = "sha256-IBzX7WASluoxXVdWkoJHRIQQF4Fi7IJOvRfRl+W3YZI=";
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
  firefox-ui-fix = {
    pname = "firefox-ui-fix";
    version = "f976adba196501dc0c8c9f4182ce4440a9bc1781";
    src = fetchFromGitHub {
      owner = "black7375";
      repo = "Firefox-UI-Fix";
      rev = "f976adba196501dc0c8c9f4182ce4440a9bc1781";
      fetchSubmodules = false;
      sha256 = "sha256-OP+gD4sJWfGSjZu2yGkkWct7A0YqVcwE+EmNDixAVGs=";
    };
    date = "2024-04-29";
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
    version = "v5.21.0";
    src = fetchFromGitHub {
      owner = "richardwilkes";
      repo = "gcs";
      rev = "v5.21.0";
      fetchSubmodules = false;
      sha256 = "sha256-mes1aXh4R1re4sW3xYDWtSIcW7lwkWoAxbcbdyT/W+o=";
    };
  };
  ohmyzsh = {
    pname = "ohmyzsh";
    version = "668ca3a32dae5ff5d164fc3be565f1e2ece248db";
    src = fetchFromGitHub {
      owner = "ohmyzsh";
      repo = "ohmyzsh";
      rev = "668ca3a32dae5ff5d164fc3be565f1e2ece248db";
      fetchSubmodules = false;
      sha256 = "sha256-Rpqfwfs2MxNtSI5rX7XNx0oXExDgf7RAGR7nN8JAayY=";
    };
    date = "2024-05-03";
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
  stumpwm = {
    pname = "stumpwm";
    version = "11be454df8f01f2ed7aa20cffe23f16f1d7b7bd6";
    src = fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm";
      rev = "11be454df8f01f2ed7aa20cffe23f16f1d7b7bd6";
      fetchSubmodules = false;
      sha256 = "sha256-aEacyvOv0mOCoPyrPOoZHhCsLNCMp+VSLr9mfamuk8A=";
    };
    date = "2024-04-18";
  };
  stumpwm-contrib = {
    pname = "stumpwm-contrib";
    version = "042a9fcb053839f4b1527d2e6f4baf33e2d16434";
    src = fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm-contrib";
      rev = "042a9fcb053839f4b1527d2e6f4baf33e2d16434";
      fetchSubmodules = false;
      sha256 = "sha256-idSdD/IXPOJXAsZSJgXbpRBH7DelEVz3xUTYh/9ZtLk=";
    };
    date = "2024-04-18";
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
