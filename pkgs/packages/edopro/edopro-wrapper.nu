const ASSETS_TO_COPY = [
  "config"
  "deck"
  "COPYING.txt"
  "expansions"
  "lflists"
  "notices"
  "puzzles"
  "fonts"
  "script"
  "skin"
  "sound"
  "textures"
];

def update_edopro_dir [edopro_dir: path, edopro_assets_dir: path] {
  mkdir $edopro_dir

  for $subpath in $ASSETS_TO_COPY {
    ^cp --dereference --recursive --update=none ($edopro_assets_dir | path join $subpath) $edopro_dir
  }

  chmod -R u+w,go-rwx $edopro_dir
  rm ($edopro_dir | path join "config/io.github.edo9300.EDOPro.desktop.in")
}

def --wrapped main [edopro_assets_dir: path, ...rest] {
  let edopro_dir = $env.XDG_DATA_HOME? | default ~/.local/share | path join edopro
  update_edopro_dir $edopro_dir $edopro_assets_dir

  exec ygopro -C ($edopro_dir | path expand) ...$rest
}
