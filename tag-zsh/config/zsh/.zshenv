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
  --color=bg+:#073642,bg:#002b36,spinner:#719e07,hl:#586e75
  --color=fg:#839496,header:#586e75,info:#cb4b16,pointer:#719e07
  --color=marker:#719e07,fg+:#839496,prompt:#719e07,hl+:#719e07
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
export PATH=$(brew --prefix openssl)/bin:$PATH
export CPATH=/usr/local/opt/openssl/include
export LIBRARY_PATH=/usr/local/opt/openssl/lib
# export PATH="/usr/local/opt/llvm/bin:$PATH"
# export LIBCLANG_PATH=/usr/local/opt/llvm/clang/5.0.0/lib
export GPG_AGENT_INFO=x
