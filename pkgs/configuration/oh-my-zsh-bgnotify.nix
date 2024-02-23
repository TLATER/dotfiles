{
  sources,
  stdenv,
  libnotify,
}:
stdenv.mkDerivation {
  inherit (sources.ohmyzsh) version src;
  pname = "oh-my-zsh-bgnotify";

  # FIXME: This breaks the notification-application detection
  # "algorithm". Notably, since libnotify will always be "detected",
  # kdialog/notifu will never work.
  #
  # It'd make more sense to add a final default implementation.
  installPhase = ''
    mkdir -p $out/
    substitute plugins/bgnotify/bgnotify.plugin.zsh $out/bgnotify.plugin.zsh \
        --replace '(( $${+commands[notify-send]} ))' 'true' \
        --replace 'notify-send' '${libnotify}/bin/notify-send'
  '';
}
