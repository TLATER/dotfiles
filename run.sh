PRELUDE_LINK="https://github.com/bbatsov/prelude/raw/master/utils/installer.sh"
TERN_REPO="https://github.com/ternjs/tern.git"

if [ ! -d "$TMPDIR" ]; then
    TMPDIR="/tmp"
fi

for folder in *; do
    if [ -d "$folder" ]; then
        read -p "Do you want to stow $folder? (y|n) " -n 1 -r
        echo
        if [[ "$REPLY" =~ ^[Yy]$ ]]; then
            # Emacs specific installation
            if [ "$folder" == "emacs" ]; then
                read -p "Do you want to install prelude? (y|n) " -n 1 -r
                echo
                if [[ "$REPLY" =~ ^[Yy]$ ]]; then
                    wget -P "$TMPDIR" "$PRELUDE_LINK"
                    chmod +x "/tmp/installer.sh"
                    /tmp/installer.sh
                fi

                read -p "Do you want to install tern? (y|n) " -n 1 -r
                echo
                if [[ "$REPLY" =~ ^[Yy]$ ]]; then
                    git clone "$TERN_REPO" "$HOME/.emacs.d/.tern"
                    cd "$HOME/.emacs.d/.tern" && npm install
                fi
            fi

            stow -t ~ "$folder"
        fi
    fi
done
