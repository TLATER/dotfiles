#!/usr/bin/env nu

let shell_files = (
  ls **/*.sh **/*.zsh | get name
)

# Statix doesn't support checking multiple files, so we need to
# awkwardly double-define a list of globs that we don't want to match
let generated_nix_files = [
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

let linters = [
  ([shellcheck] ++ $shell_files)
  ([nixfmt --check --strict] ++ $nix_files)
  ([deadnix --fail] ++ $nix_files)
  ([statix check --ignore] ++ $generated_nix_files)
]

def run-lint [command] {
  try {
    print $'Running ($command.0)...'
    ^$command.0 ...($command | skip 1)
    0
  } catch {|e|
    $e.exit_code
  }
}

mkdir $env.out

let results = $linters | each {|command| run-lint $command}

print 'Linter results:'

$results | zip $linters | each {|| match $in.0 {
  0 => {print $'(ansi green)($in.1.0)(ansi reset)'}
  _ => {print $'(ansi red)($in.1.0)(ansi reset)'}
}}

if ($results | any {|exit_code| $exit_code != 0}) {
  exit 1
}
