PATH=$HOME/bin:${ZDOTDIR:-$HOME}/.zprofile:$PATH
EDITOR='vim'
VISUAL='code --wait'

export FZF_DEFAULT_COMMAND='fd --type file --color=always'
export FZF_DEFAULT_OPTS="--ansi"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG='f:finder;o:fzopen;p:preview-tui'

export CLICOLOR=1
export TERM=xterm-256color
