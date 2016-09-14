for folder in *; do
    if [ -d "$folder" ]; then
        read -p "Do you want to stow $folder? (y|n) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            stow -t ~ "$folder"
        fi
    fi
done
