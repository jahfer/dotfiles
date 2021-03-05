#!/bin/bash
panic() {
  printf "%s: %.0s${1}\n" "${0##*/}" "$@" >&2
  exit 1
}

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

SPIN_EMACS_PATH="${HOME}/.emacs.d"
mkdir -p $SPIN_EMACS_PATH || panic "Failed to mkdir -p ${SPIN_EMACS_PATH}"
[ -L ${SPIN_EMACS_PATH}/init.el ] || ln -s ${DOTFILES}/emacs/init.el ${SPIN_EMACS_PATH}/init.el || panic "Failed to link emacs init.el"
[ -L ${SPIN_EMACS_PATH}/scratch-template ] || ln -s ${DOTFILES}/emacs/scratch-template ${SPIN_EMACS_PATH}/scratch-template || panic "Failed to link emacs scratch-template"

# # vim
# ln -s ${BASEDIR}/vimrc ~/.vimrc
# ln -s ${BASEDIR}/vim/ ~/.vim

# # zsh
# ln -s ${BASEDIR}/zshrc ~/.zshrc

# # git
# ln -s ${BASEDIR}/gitconfig ~/.gitconfig
