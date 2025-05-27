def readable-pwd [] {
  match ($env.PWD | path relative-to $nu.home-path) {
    null => $env.PWD
    '' => '~'
    $relative_pwd => ([~ $relative_pwd] | path join)
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

  hooks: {
    pre_execution: [{||
      $env.repl_commandline = (commandline)
      # Using external echo since nushell doesn't support
      # printing raw bytes, and its `ansi` command is too
      # limited
      #
      # The bytes to send are 0x21 + k and 0x21 + \, however
      # nushell only supports 0x21 + ]

      # Sets the tab status (title shown in screen's menus)
      ^echo -en $'\x1bk(readable-pwd) $ ($env.repl_commandline)\x1b\\'

      # Sets the hardstatus (title shown in window manager)
      ^echo -en $'\x1b]0;(readable-pwd) $ ($env.repl_commandline)\a'
    }]
  }

  buffer_editor: [emacslcient -tc]
}
