{ stdenv, lua5_4 }:
lua5_4.overrideAttrs (old: {
  makeFlags = old.makeFlags ++ [ "CC=${stdenv.cc.targetPrefix}c++" ];
})
