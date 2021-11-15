#!/bin/bash
panic() {
  printf "%s: %.0s${1}\n" "${0##*/}" "$@" >&2
  exit 1
}

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# packages to downlaod
TIG_VERSION="2.5.3"
NNN_VERSION="3.5"

# Create executable folder
mkdir -p $HOME/bin

# Install zsh config
if [ ! -d "${ZDOTDIR:-$HOME}/.zprezto" ]; then
  git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto" || panic "Failed to download zprezto"
fi
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

  wget -O - "https://github.com/jonas/tig/releases/download/tig-${TIG_VERSION}/tig-${TIG_VERSION}.tar.gz" | tar -zxf - || panic "Failed to download tig"

  if [ ! -d "/home/spin/bin/tig" ]; then
    cd ./tig-${TIG_VERSION}
    make || panic "Failed to make tig"
    make install || panic "Failed to install tig"
  fi
fi
[ -L $HOME/.tigrc ] || ln -s ${DOTFILES}/tig/tigrc ${HOME}/.tigrc || panic "Failed to symlink .tigrc"

# Install nnn
if ! command -v nnn &> /dev/null; then
  sudo apt-get install -y pkg-config libncursesw5-dev libreadline-dev || panic "Failed to install dependencies for nnn"
  wget -O - "https://github.com/jarun/nnn/releases/download/v${NNN_VERSION}/nnn-v${NNN_VERSION}.tar.gz" | tar -zxf - || panic "Failed to download nnn"
  cd ./nnn-${NNN_VERSION}
  sudo make strip install
fi

# Install fzf
if ! command -v fzf &> /dev/null; then
  sudo apt-get install fzf
fi

# Install fd
if ! command -v fdfind &> /dev/null; then
  sudo apt-get install fd-find
  sudo ln -s $(which fdfind) /usr/local/bin/fd || panic "Failed to symlind fdfind"
fi

# Install bat
if ! command -v batcat &> /dev/null; then
  sudo apt-get install -o Dpkg::Options::="--force-overwrite" bat
fi

# Install ripgrep
if ! command -v rg &> /dev/null; then
  sudo apt-get install ripgrep
fi

# tmux
[ -L "$HOME/.tmux.conf" ] || ln -s ${DOTFILES}/tmux/tmux.conf ${HOME}/.tmux.conf || panic "Failed to symlink .tmux.conf"

# git
[ -L "$HOME/.gitconfig" ] || [ -f "$HOME/.gitconfig" ] || ln -s ${DOTFILES}/git/gitconfig ${HOME}/.gitconfig || panic "Failed to symlink .gitconfig"
