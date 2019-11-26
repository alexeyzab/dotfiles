cdpath=( "$HOME" "$HOME/code" $cdpath )

fpath=( "$ZDOTDIR/functions" $fpath )

export HISTCONTROL=ignoreboth:erasedups
export VISUAL='TERM=xterm-24bits emacs -nw'
export EDITOR='TERM=xterm-24bits emacs -nw'
export BROWSER='chromium-browser'
export MANWIDTH=80
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export FZF_DEFAULT_OPTS='
  --color fg:-1,bg:-1,hl:230,fg+:3,bg+:233,hl+:229
  --color info:150,prompt:110,spinner:150,pointer:167,marker:174
'
export FZF_DEFAULT_COMMAND='rg --files --hidden --smart-case --glob "!.git/*"'
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export ZDOTDIR="$HOME"/.config/zsh
source $ZDOTDIR/.aliases
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export LESSHISTFILE="$XDG_CACHE_HOME"/lesshist
export GOPATH="$HOME"/code/go
export PATH=~/.npm-global/bin:$PATH
export PATH=~/.cargo/bin:$PATH
export PATH=~/.cargo/env:$PATH
export PATH=~/.venv/bin:$PATH
export PATH=~/.local/bin:$PATH
export PATH=~/.cabal/bin:$PATH
export PATH=~/.nix-profile/bin:$PATH
export PATH=/Users/alexeyzab/Library/Python/3.7/bin:$PATH
export XDG_CURRENT_DESKTOP=Unity
export GPG_AGENT_INFO=x
