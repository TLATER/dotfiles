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
  firefox-ui-fix = {
    pname = "firefox-ui-fix";
    version = "4ee131b29b5e52cd0da09cfb5a61d81838e5be9c";
    src = fetchFromGitHub {
      owner = "black7375";
      repo = "Firefox-UI-Fix";
      rev = "4ee131b29b5e52cd0da09cfb5a61d81838e5be9c";
      fetchSubmodules = false;
      sha256 = "sha256-ks17bMC/I/Mqg4s5utnRIJ+UvwpCnXw8e10ApTCbsJI=";
    };
    date = "2023-06-15";
  };
  gauth = {
    pname = "gauth";
    version = "v1.2.2";
    src = fetchFromGitHub {
      owner = "pcarrier";
      repo = "gauth";
      rev = "v1.2.2";
      fetchSubmodules = false;
      sha256 = "sha256-EprxybxUEvpT4jUaI75Y4E3nzskjX8HA9qJap6/Ws+0=";
    };
  };
  ohmyzsh = {
    pname = "ohmyzsh";
    version = "42b86327ed875ee182f8fc394b90ae9328a5ac00";
    src = fetchFromGitHub {
      owner = "ohmyzsh";
      repo = "ohmyzsh";
      rev = "42b86327ed875ee182f8fc394b90ae9328a5ac00";
      fetchSubmodules = false;
      sha256 = "sha256-dWDOuxtFqrCO0dwQ+kXgPwrLegdBRrqOxhFKqe2SFLs=";
    };
    date = "2023-06-16";
  };
  phosphor-icons = {
    pname = "phosphor-icons";
    version = "d3b385a38acbb8b16a068e8e2515660af6978cf9";
    src = fetchFromGitHub {
      owner = "phosphor-icons";
      repo = "web";
      rev = "d3b385a38acbb8b16a068e8e2515660af6978cf9";
      fetchSubmodules = false;
      sha256 = "sha256-zq7z2IEH6Cfv/3O1jYe1MYbvZKb+FicGf4QFDlbseTI=";
    };
    date = "2023-03-12";
  };
  stumpwm = {
    pname = "stumpwm";
    version = "9072f22e1b0fa6a9ce0e7658713fb61a9ddb1469";
    src = fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm";
      rev = "9072f22e1b0fa6a9ce0e7658713fb61a9ddb1469";
      fetchSubmodules = false;
      sha256 = "sha256-GjTFUQ+qr6zXCxhSi8PqsWuBCKkb/RRfDkhzoBo9lBA=";
    };
    date = "2023-04-29";
  };
  stumpwm-contrib = {
    pname = "stumpwm-contrib";
    version = "7d1e57ca1e011e0e56059ebf6e833e0976904d2a";
    src = fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm-contrib";
      rev = "7d1e57ca1e011e0e56059ebf6e833e0976904d2a";
      fetchSubmodules = false;
      sha256 = "sha256-EVjl3m6dfyT/84SJ5Qp95odkMRXwerSut524Is4IV8k=";
    };
    date = "2023-05-27";
  };
  tridactyl-emacs = {
    pname = "tridactyl-emacs";
    version = "f0b84500117bde9bf008fb25c264aa3386b6c07f";
    src = fetchFromGitHub {
      owner = "jumper047";
      repo = "tridactyl_emacs_config";
      rev = "f0b84500117bde9bf008fb25c264aa3386b6c07f";
      fetchSubmodules = false;
      sha256 = "sha256-+F5ENKOJXnUCH2kEFARhc2ieChUXNn0IIdCX/cjUgHE=";
    };
    date = "2023-03-17";
  };
  zsh-background-notify = {
    pname = "zsh-background-notify";
    version = "d5f0430cb052f82c433c17707816910da87e201e";
    src = fetchFromGitHub {
      owner = "t413";
      repo = "zsh-background-notify";
      rev = "d5f0430cb052f82c433c17707816910da87e201e";
      fetchSubmodules = false;
      sha256 = "sha256-NhDAOKCkNItODP8d68FVySZghTvPKXG2eBPlvkCZDl0=";
    };
    date = "2015-09-14";
  };
}
