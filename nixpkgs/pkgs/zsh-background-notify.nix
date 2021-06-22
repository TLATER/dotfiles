{ sources, stdenv, libnotify }:

stdenv.mkDerivation {
  inherit (sources.zsh-background-notify) pname version src;

  # FIXME: This breaks the notification-application detection
  # "algorithm". Notably, since libnotify will always be "detected",
  # kdialog/notifu will never work.
  #
  # It'd make more sense to add a final default implementation.
  installPhase = ''
    mkdir -p $out/
    substitute bgnotify.plugin.zsh $out/bgnotify.plugin.zsh \
        --replace 'hash notify-send' 'true' \
        --replace 'notify-send' '${libnotify}/bin/notify-send'
  '';
}
