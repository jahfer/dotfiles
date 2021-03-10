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

if ! command -v tig &> /dev/null; then
  [ dpkg -s libncurses5-dev ] || sudo apt-get install -y libncurses5-dev
  [ dpkg -s libncursesw5-dev ] || sudo apt-get install -y libncursesw5-dev

  if [ ! -d "/src/github.com/jonas/tig" ]; then
    mkdir -p /src/github.com/jonas && cd /src/github.com/jonas
    git clone https://github.com/jonas/tig.git || panic "Failed to clone jonas/tig"
  fi

  if [ ! -d "/home/spin/bin/tig" ]; then
    cd /src/github.com/jonas/tig
    make || panic "Failed to make tig"
    make install || panic "Failed to install tig"
  fi

  [ -L /usr/local/bin/tig ] || ln -s /home/spin/bin/tig /usr/local/bin/tig || panic "Failed to symlink tig"
fi
[ -L ${HOME}/.tigrc ] || ln -s ${DOTFILES}/tig/.tigrc || panic "Failed to link .tigrc"
# # vim
# ln -s ${BASEDIR}/vimrc ~/.vimrc
# ln -s ${BASEDIR}/vim/ ~/.vim

# # zsh
# ln -s ${BASEDIR}/zshrc ~/.zshrc

# # git
# ln -s ${BASEDIR}/gitconfig ~/.gitconfig
