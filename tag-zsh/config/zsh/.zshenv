cdpath=( "$HOME" "$HOME/code" $cdpath )

fpath=( "$ZDOTDIR/functions" $fpath )

export HISTCONTROL=ignoreboth:erasedups
export VISUAL='emacs -nw'
export EDITOR='emacs -nw'
export BROWSER='google-chrome-beta'
export GPG_TTY="$(tty)"
export MANWIDTH=80
export PATH="/usr/local/opt/sqlite/bin:$PATH"
export FZF_DEFAULT_OPTS='
    --color fg:-1,bg:-1,hl:230,fg+:3,bg+:233,hl+:229
    --color info:150,prompt:110,spinner:150,pointer:167,marker:174
    '
export XDG_CACHE_HOME="$HOME"/.cache
export XDG_CONFIG_HOME="$HOME"/.config
export XDG_DATA_HOME="$HOME"/.local/share
export ZDOTDIR="$HOME"/.config/zsh
source $ZDOTDIR/.aliases
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export LESSHISTFILE="$XDG_CACHE_HOME"/lesshist
export NOTITLE=1 # avoid broken grml precmd hook
export PATH=~/.npm-global/bin:$PATH
export PATH=~/usr/share/java/languagetool:$PATH
export PATH=~/.cargo/bin:$PATH
export PATH=~/.cargo/env:$PATH
export RUST_SRC_PATH=~/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src
export PATH=~/code/cards-with-comrades/bin:$PATH
export PATH=~/.venv/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=~/code/yesod-pastebin/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
export PATH=/Library/TeX/texbin:$PATH
export PATH=~/.local/bin:$PATH
export GPG_AGENT_INFO=x
