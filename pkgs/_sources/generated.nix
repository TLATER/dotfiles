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
    version = "8a3d8f5a38dd0584fab36cf5b6bed0753e798421";
    src = fetchFromGitHub {
      owner = "black7375";
      repo = "Firefox-UI-Fix";
      rev = "8a3d8f5a38dd0584fab36cf5b6bed0753e798421";
      fetchSubmodules = false;
      sha256 = "sha256-8uu6GaaCRwqseMwJ64kGZvGHBYpAvxXcwYJ9zhsuCWw=";
    };
    date = "2023-06-26";
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
    version = "fe4b5659863c388786986d70fa6d1bb66b00afb6";
    src = fetchFromGitHub {
      owner = "ohmyzsh";
      repo = "ohmyzsh";
      rev = "fe4b5659863c388786986d70fa6d1bb66b00afb6";
      fetchSubmodules = false;
      sha256 = "sha256-fzAFlYQ5fkECnAUHx3JkNlJTMYNaB248OJxiEFiuLis=";
    };
    date = "2023-06-30";
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
    version = "63adb4ed3c2f8118e9a32fadf4371d938b0a8b62";
    src = fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm";
      rev = "63adb4ed3c2f8118e9a32fadf4371d938b0a8b62";
      fetchSubmodules = false;
      sha256 = "sha256-ZFvlGAhBx7vuoiyY6esXLWLehHRZdvTnQh8Qv6vYprk=";
    };
    date = "2023-06-26";
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
