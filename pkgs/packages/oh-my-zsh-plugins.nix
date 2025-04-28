{ sources, runCommand }:
runCommand "oh-my-zsh-plugins" { } ''
  install -Dt $out/screen/ ${sources.ohmyzsh.src}/plugins/screen/*sh
  install -Dt $out/emacs/ ${sources.ohmyzsh.src}/plugins/emacs/*sh
''
