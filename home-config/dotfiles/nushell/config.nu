$env.config.show_banner = false

def --env eproject [] {
  let cmd = '
    (with-current-buffer (window-buffer)
      (let ((project (project-current)))
        (when project
          (expand-file-name (project-root project)))))
    '

  let project = emacsclient --eval $cmd | str trim --char '"'
  cd $project
}
