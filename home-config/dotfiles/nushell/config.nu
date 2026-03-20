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

alias ls-builtin = ls
def ls [
  --all (-a),         # Show hidden files
  --long (-l),        # Get all available columns for each entry
  --short-names (-s), # Only print the file names, and not the path
  --full-paths (-f),  # display paths as absolute paths
  --du (-d),          # Display the apparent directory size ("disk usage") in place of the directory metadata size
  --directory (-D),   # List the specified directory itself instead of its contents
  --mime-type (-m),   # Show mime-type in type column instead of 'file' (based on filenames only; files' contents are not examined)
  --threads (-t),     # Use multiple threads to list contents. Output will be non-deterministic.
  ...pattern: glob,   # The glob pattern to use.
]: [ nothing -> table ] {
  let pattern = if ($pattern | is-empty) { [ '.' ] } else { $pattern }
  (ls-builtin
      --all=$all
      --long
      --short-names=$short_names
      --full-paths=$full_paths
      --du=$du
      --directory=$directory
      --mime-type=$mime_type
      --threads=$threads
      ...$pattern
  ) | sort-by type name --natural | reject ...(if $long { [ ] } else  [type readonly num_links inode created accessed])
}

def nix-create-gcroot [path: path] {
  let store_path = nix-store --add-fixed sha256 $path
  let name = $store_path | path basename | split row '-' | skip 1 | str join '-'
  let gcroot = [(pwd) gcroots ($name + '.gcroot')] | path join

  if ($gcroot | path exists) {
    print gcroot already exists
    return
  }

  mkdir (dirname $gcroot)
  nix-store --add-root $gcroot --indirect --realise $store_path
}

def git-prune-merged [] {
  let merged = git branch --merged | lines | where ($it != "* master" and $it != "* main")

  if ($merged | length) == 0 {
    print 'No branches to prune'
    return
  }

  print 'Branches to prune:'
  print $merged

  print 'Really prune [y/n]?'

  loop {
    match (input listen --types [key]).code {
      'y' => {
        $merged | each {|br| git branch -D ($br | str trim) } | str trim
        break
      }
      'n' => {
        break
      }
    }
  }
}

def get-project-root []: [nothing -> directory nothing -> nothing] {
  (
    git rev-parse --show-toplevel | complete
    | if $in.exit_code == 0 { $in.stdout | str trim } else { null }
  )
}

$env.PROMPT_COMMAND = {||
  let project_root = get-project-root
  let relative_root = (
    $project_root
    | default (do -i { $env.PWD | path relative-to $nu.home-path; echo $nu.home-path })
    | default '/'
  )

  let home_char = match $relative_root {
    '/' => '/'
    $root if $root == $nu.home-path => '~'
    _ => $'$(basename $project_root)'
  }

  let dir = $env.PWD | str replace $relative_root $home_char

  let path_color = (
    if (is-admin) {
      ansi red_bold
    } else if ($project_root != null) {
      ansi purple_bold
    } else {
      ansi green_bold
    }
  )
  let separator_color = (
    if (is-admin) {
      ansi light_red_bold
    } else if ($project_root != null) {
      ansi light_purple_bold
    } else {
      ansi light_green_bold
    }
  )
  let path_segment = $"($path_color)($dir)(ansi reset)"

  $path_segment | str replace --all (char path_sep) $"($separator_color)(char path_sep)($path_color)"
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

  buffer_editor: [emacsclient -tc]
}
