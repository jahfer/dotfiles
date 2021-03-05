#!/bin/bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

mkdir -p ~/.emacs.d
[ -L ~/.emacs.d/init.el ] || ln -s ~/emacs/init.el

# # vim
# ln -s ${BASEDIR}/vimrc ~/.vimrc
# ln -s ${BASEDIR}/vim/ ~/.vim

# # zsh
# ln -s ${BASEDIR}/zshrc ~/.zshrc

# # git
# ln -s ${BASEDIR}/gitconfig ~/.gitconfig
