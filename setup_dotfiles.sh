#!/bin/bash
# Simple script for configuration of dotfiles.
# -r option reset's elpa packages

function remove_emacs_directory {
    if [ -L ~/.emacs.d ]; then
        rm ~/.emacs.d
    fi

    # if it's a directory, remove the directory and it's files
    if [ -d ~/.emacs.d/ ]; then
        rm -r -f ~/.emacs.d/
    fi
}

function backup_elpa {
    if [ ! -d ~/.elpa.backup/ ]; then
        mkdir ~/.elpa.backup/
    fi

    if [ -d ~/.emacs.d/elpa/ ]; then
        cp -rf ~/.emacs.d/elpa/ ~/.elpa.backup/
    fi
}

function copy_emacs_folder_to_home {
    cp -r -f ./.emacs.d ~/
}

# execute main logic
backup_elpa
remove_emacs_directory
copy_emacs_folder_to_home

# maybe restore elpa folder
TEMP=`getopt -o r:: \
     -n 'example.bash' -- "$@"`
if [ $? != 0 ] ; then echo "Terminating..." >&2 ; exit 1 ; fi

eval set -- "$TEMP"

while true; do
    case "$1" in
        -r)
            cp -rf ~/.elpa.backup ~/.emacs.d/ ;
            mv -f ~/.emacs.d/.elpa.backup/ ~/.emacs.d/elpa/ ;
            echo "restoring elpa backup" ;
            shift 2 ;;
        --) shift; break ;;
    esac
done
