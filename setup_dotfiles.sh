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

function make_symlinks {
    destdir=`pwd`/.emacs.d/
    mkdir ~/.emacs.d/ && cd ~/.emacs.d/

    ln -sf $destdir/init.el
    ln -sf $destdir/custom.el
    ln -sf $destdir/ac-dict/
    ln -sf $destdir/lisp/
    ln -sf $destdir/vendor/
    ln -sf $destdir/snippets/
}

# execute main logic
remove_emacs_directory
make_symlinks
