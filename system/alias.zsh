# git
alias gco="git checkout"

# ruby/rails
alias bx="bundle exec"
alias rt="bundle exec ruby -Itest"

# tmux
alias tc="tmux -CC"
alias ta="tmux -CC attach"

# search
alias f="rg --files | fzf --preview 'batcat --color=always --style=numbers --line-range=:500 {}'"
