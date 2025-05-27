{ sources, melpaBuild }:
melpaBuild {
  inherit (sources.eglot-x) pname src;
  # Build a MELPA unstable version string - this is the date with no
  # separators followed by the hour/minute of the commit. We don't
  # have the latter so we set it to 0.
  version = builtins.replaceStrings [ "-" ] [ "" ] sources.eglot-x.date + ".0";
}
