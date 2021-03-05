#!/bin/bash

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

EMACS_PATH = ~/.emacs.d
mkdir -p ${SPIN_EMACS_PATH}
[ -L ${SPIN_EMACS_PATH}/init.el ] || ln -s ${DOTFILES}/emacs/init.el ${SPIN_EMACS_PATH}/init.el
[ -L ${SPIN_EMACS_PATH}/scratch-template ] || ln -s ${DOTFILES}/emacs/scratch-template ${SPIN_EMACS_PATH}/scratch-template

# # vim
# ln -s ${BASEDIR}/vimrc ~/.vimrc
# ln -s ${BASEDIR}/vim/ ~/.vim

# # zsh
# ln -s ${BASEDIR}/zshrc ~/.zshrc

# # git
# ln -s ${BASEDIR}/gitconfig ~/.gitconfig
