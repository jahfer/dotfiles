#!/bin/bash
panic() {
  printf "%s: %.0s${1}\n" "${0##*/}" "$@" >&2
  exit 1
}

DOTFILES="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# packages to downlaod
TIG_VERSION="2.5.3"
NNN_VERSION="4.3"
DELTA_VERSION="0.12.1"

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
  wget -O - "https://github.com/jonas/tig/releases/download/tig-${TIG_VERSION}/tig-${TIG_VERSION}.tar.gz" | tar -zxf - || panic "Failed to download tig"

  if [ ! -d "${HOME}/bin/tig" ]; then
    cd ./tig-${TIG_VERSION}
    make || panic "Failed to make tig"
    make install || panic "Failed to install tig"
  fi
fi
[ -L $HOME/.tigrc ] || ln -s ${DOTFILES}/tig/tigrc ${HOME}/.tigrc || panic "Failed to symlink .tigrc"


cd "${DOTFILES}" || panic "Failed to cd into dotfiles directory"

# Install nnn
if ! command -v nnn &> /dev/null; then
  wget -O - "https://github.com/jarun/nnn/releases/download/v${NNN_VERSION}/nnn-v${NNN_VERSION}.tar.gz" | tar -zxf - || panic "Failed to download nnn"
  cd ./nnn-${NNN_VERSION} || panic "Failed to cd into nnn directory"
  sudo make strip install || panic "Failed to install nnn"

  curl -Ls "https://raw.githubusercontent.com/jarun/nnn/v${NNN_VERSION}/plugins/getplugs" | sh || panic "Failed to install nnn plugins"
fi

# Install fzf
if ! command -v fzf &> /dev/null; then
  sudo apt-get -o DPkg::Lock::Timeout=60 install -y fzf
fi

# Install fd
if ! command -v fdfind &> /dev/null; then
  # sudo apt-get -o DPkg::Lock::Timeout=60 install -y fd-find
  # sudo ln -s $(which fdfind) "${HOME}/bin/fd" || panic "Failed to symlind fdfind"
fi

# Install bat
if ! command -v batcat &> /dev/null; then
  # sudo apt-get -o DPkg::Lock::Timeout=60 -o Dpkg::Options::="--force-overwrite" install -y bat
fi

# Install ripgrep
if ! command -v rg &> /dev/null; then
  # sudo apt-get -o DPkg::Lock::Timeout=60 install ripgrep
fi

# Install delta
if ! command -v delta &> /dev/null; then
  # wget "https://github.com/dandavison/delta/releases/download/${DELTA_VERSION}/git-delta_${DELTA_VERSION}_amd64.deb" || panic "Failed to download git-delta"
  # sudo dpkg -i git-delta_${DELTA_VERSION}_amd64.deb
fi

# Config Files

# tmux
[ -L "$HOME/.tmux.conf" ] || ln -s ${DOTFILES}/tmux/tmux.conf ${HOME}/.tmux.conf || panic "Failed to symlink .tmux.conf"

# git
rm -f ${HOME}/.gitconfig && ln -s ${DOTFILES}/git/gitconfig ${HOME}/.gitconfig || panic "Failed to symlink .gitconfig"


# Symbolic link of fdfind -> fd
if ! command -v fd &> /dev/null; then
  ln -s "$(which fdfind)" "${HOME}/bin/fd" || panic "Failed to symlink fdfind as fd"
fi

# batcat
if command -v batcat &> /dev/null; then
  ln -s "$(which batcat)" "${HOME}/bin/bat" || panic "Failed to symlink batcat as bat"
fi
