#!/bin/bash
# Simple script for configuration of dotfiles.

# if it's a symbolic link, remove the file representing it
if [ -L ~/.emacs.d ]; then
    rm ~/.emacs.d
fi

# if it's a directory, remove the directory and it's files
if [ -d ~/.emacs.d/ ]; then
    rm -r -f ~/.emacs.d/
fi

# cp'ing .emacs.d to bash
cp -r -f ./.emacs.d ~/
