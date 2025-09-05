use std/log

$env.NU_LOG_LEVEL = match ($env.VERBOSE?) {
  "1" => "DEBUG"
  _ => "INFO"
}

let profile_path = "~/.local/state/nix/profiles/" | path expand --no-symlink
log info $"Cleaning up home-manager profiles in ($profile_path)"

let to_delete = (
  ls $profile_path | sort-by modified --reverse | skip 6
)

if $to_delete == [] {
  log info $"No obsolete generations to delete"
  exit 0
}

log info $"Deleting obsolete generations:(
  $to_delete.name
  | each {|d| '- ' | str join }
  | str join (char newline)
)"

def delete_generation [file] {
  if $env.DRY_RUN? != null {
    log warning $"Skipping deletion for ($file)"
  } else {
    log debug $"Deleting generation ($file)"
    rm $file
  }
}

$to_delete | each {|g| delete_generation $g.name }
