#!/bin/bash

BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# emacs
mkdir -p ~/.emacs.d
if [ ! -f ~/.emacs.d/init.el ]; then
  ln -s ${BASEDIR}/emacs/init.el
fi
