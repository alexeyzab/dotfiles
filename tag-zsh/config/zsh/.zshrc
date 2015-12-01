# load custom executable functions
for function in ~/.config/zsh/functions/*; do
  source $function
done

# extra files in ~/.config/zsh/configs/pre , ~/.config/zsh/configs , and ~/.config/zsh/configs/post
# these are loaded first, second, and third, respectively.
_load_settings() {
  _dir="$1"
  if [ -d "$_dir" ]; then
    if [ -d "$_dir/pre" ]; then
      for config in "$_dir"/pre/**/*(N-.); do
        . $config
      done
    fi

    for config in "$_dir"/**/*(N-.); do
      case "$config" in
        "$_dir"/pre/*)
          :
          ;;
        "$_dir"/post/*)
          :
          ;;
        *)
          if [ -f $config ]; then
            . $config
          fi
          ;;
      esac
    done

    if [ -d "$_dir/post" ]; then
      for config in "$_dir"/post/**/*(N-.); do
        . $config
      done
    fi
  fi
}
_load_settings "$ZDOTDIR"/configs

# aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# tmuxline workaround
if pgrep "tmux" > /dev/null
  then
    tmux source-file ~/.tmux.snapshot
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# FZF color scheme
export FZF_DEFAULT_OPTS='
  --color=dark,hl:33,hl+:37,fg+:235,bg+:136
  --color info:254,prompt:37,spinner:108,pointer:235,marker:235
'


# NeoVim truecolor support
export NVIM_TUI_ENABLE_TRUE_COLOR=1

# Fix for Weechat special characters
export LANG=en_US.UTF-8 LC_CTYPE="en_US.UTF-8"

# chruby setup
RUBIES=(/home/alexeyzab/.rubies/rub*)
source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh

# Zsh syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Add scripts folder to path
export PATH="$HOME/scripts/:$PATH"

export BROWSER='google-chrome-stable'
export GPG_TTY="$(tty)"
export MANWIDTH=80

setopt vi
