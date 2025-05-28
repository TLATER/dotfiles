#!/usr/bin/env nu

let shell_files = (
  ls **/*.sh **/dashrc |
  where type == file |
  get name
)

let nix_files = (
  ls **/*.nix |
  where type == file |
  where name !~ "hardware-configuration.nix$|^pkgs/_sources|emacs.d/share/templates" |
  get name
)

let linters = [
  ([shellcheck] ++ $shell_files)
  ([nixfmt --check --strict] ++ $nix_files)
  ([deadnix --fail] ++ $nix_files)
  ([statix check] ++ $nix_files)
]

def run-lint [command] {
  print $'Running ($command.0)...'

  let exit_code = try {
    ^$command.0 ...($command | skip 1)
    $env.LAST_EXIT_CODE
  } catch {|e|
    $e.exit_code
  }

  [$command.0, $exit_code]
}

mkdir $env.out

let results = $linters | each {|command| run-lint $command}

print 'Linter results:'

let success = $results | each {|result|
  match $result.1 {
    0 => {print $'(ansi green)($result.0)(ansi reset)'}
    _ => {print $'(ansi red)($result.0)(ansi reset)'}
  }

  $result.1
} | math sum

exit $success
