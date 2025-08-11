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
