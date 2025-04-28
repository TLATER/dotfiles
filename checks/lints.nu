#!/usr/bin/env nu

let shell_files = (
  ls **/*.sh **/*.zsh | get name
)

# Statix doesn't support checking multiple files, so we need to
# awkwardly double-define a list of globs that we don't want to match
let generated_nix_files: list<glob> = [
  'hardware-configuration.nix'
  'home-config/dotfiles/emacs.d/share/templates/*'
  'pkgs/_sources/*'
]

let nix_files = (
  ls **/*.nix |
  where name !~ ($generated_nix_files | str join '|') |
  where type == file |
  get name
)

mkdir $env.out

def run-lint [command] {
  print $'Running ($command.0)...'

  let result = run-external $command.0 ...($command | skip 1) | complete
  $result.stdout | save $'($env.out)/($command.0).log'
  $result.stderr | save --append $'($env.out)/($command.0).log'

  if $result.stdout != "" or $result.stderr != "" {
    print $result.stdout
    print --stderr $result.stderr
  }

  $result
}

let linters = [
  ([shellcheck] ++ $shell_files)
  ([nixfmt --check --strict] ++ $nix_files)
  ([deadnix ] ++ $nix_files)
]

let failed = $linters | each {|command| run-lint $command}

# Statix gets special handling because of its multi-file check
# limitation
print 'Running statix...'
do {
  statix check --ignore ...$generated_nix_files |
    tee { save ($env.out + '/statix.log')}
}

if ($failed | any {|result| $result.exit_code != 0}) or $env.LAST_EXIT_CODE != 0 {
  exit 1
}
