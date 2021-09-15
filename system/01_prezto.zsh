setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
  [ ! -e "${ZDOTDIR:-$HOME}/.${rcfile:t}" ] && ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi
