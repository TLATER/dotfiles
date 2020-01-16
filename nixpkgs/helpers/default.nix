{ lib }:

rec {
  dictToVars = dict: builtins.concatStringsSep " " (lib.mapAttrsToList (n: v: "${n}=\"${toString v}\"") dict);
}
