def readable-pwd [] {
  match ($env.PWD | path relative-to $nu.home-path) {
    null => $env.PWD
    '' => '~'
    $relative_pwd => ([~ $relative_pwd] | path join)
  }
}

def --env eproject [] {
  let cmd = '
    (with-current-buffer (window-buffer)
      (let ((project (project-current)))
        (when project
          (expand-file-name (project-root project)))))
    '

  let project = emacsclient --eval $cmd | str trim --char '"'

  if $project != nil {
    cd $project
  }
}

def nix-create-gcroot [path] {
  let store_path = nix-store --add-fixed sha256 $path
  let components = $store_path | path basename | split row '-' | {name: ($in | skip 1 | str join '-'), hash: $in.0}

  let local_reference = [(pwd) gcroots ($components.name + '.gcroot')] | path join
  let gcroot = [/nix/var/nix/gcroots/per-user/ $env.USER ($components.hash + (basename $local_reference))] | path join

  if (($local_reference | path exists) or ($gcroot | path exists)) {
    print $'gcroot already exists; you should delete:'
    print $'- ($local_reference)'
    print $'- ($gcroot)'
    return
  }

  mkdir (dirname $local_reference)

  ln -s $store_path $local_reference
  ln -s $local_reference $gcroot
}

$env.config = {
  show_banner: false

  table: {
    mode: light
    index_mode: auto
    trim: {
      methodology: truncating
      truncating_suffix: ..
    }
  }

  footer_mode: auto
  use_ansi_coloring: true

  history: {
    max_size: 1_000_000
    file_format: sqlite
    isolation: true
  }

  buffer_editor: [emacslcient -tc]
}
