# load our own completion functions
fpath=(~/.config/zsh/completion /usr/local/share/zsh/site-functions $fpath)

# completion
autoload -U compinit
compinit
