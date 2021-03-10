#!/bin/bash
panic() {
  printf "%s: %.0s${1}\n" "${0##*/}" "$@" >&2
  exit 1
}

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Create executable folder
mkdir -p $HOME/bin

# Install zsh config
ln -sf ${DOTFILES}/runcom/zshrc ${HOME}/.zshrc || panic "Failed to symlink .zshrc"

# Config Emacs
SPIN_EMACS_PATH="${HOME}/.emacs.d"
mkdir -p $SPIN_EMACS_PATH || panic "Failed to mkdir -p ${SPIN_EMACS_PATH}"
[ -L ${SPIN_EMACS_PATH}/init.el ] || ln -s ${DOTFILES}/emacs/init.el ${SPIN_EMACS_PATH}/init.el || panic "Failed to symlink emacs init.el"
[ -L ${SPIN_EMACS_PATH}/scratch-template ] || ln -s ${DOTFILES}/emacs/scratch-template ${SPIN_EMACS_PATH}/scratch-template || panic "Failed to symlink emacs scratch-template"

# Install Tig
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
fi
[ -L $HOME/.tigrc ] || ln -s ${DOTFILES}/tig/tigrc ${HOME}/.tigrc || panic "Failed to symlink .tigrc"
