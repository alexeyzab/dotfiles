# load custom executable functions
for function in ~/.config/zsh/functions/*; do
  source $function
done

# extra files in ~/.zsh/configs/pre , ~/.zsh/configs , and ~/.zsh/configs/post
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
_load_settings "$ZDOTDIR/configs"

zstyle ':completion:*:sudo:*' command-path $path

HISTSIZE=50000
SAVEHIST=$HISTSIZE
HISTFILE="$ZDOTDIR"/.zsh_history

setopt inc_append_history

# Enable emacs keybindings
bindkey -e
my-backward-delete-word() {
    local WORDCHARS=${WORDCHARS/\//}
    zle backward-delete-word
}
zle -N my-backward-delete-word
bindkey '^W' my-backward-delete-word

# Hub
eval "$(hub alias -s)"

nixup() {
    cwd_name=${PWD##*/}

    cabal2nix . > default.nix
    cabal2nix . --shell > shell.nix
    touch ./release.nix

    echo "let
      config = {
	packageOverrides = pkgs: rec {
	  haskellPackages = pkgs.haskellPackages.override {
	    overrides = haskellPackagesNew: haskellPackagesOld: rec {
	      $cwd_name =
		haskellPackagesNew.callPackage ./default.nix { };
	    };
	  };
	};
      };

    pkgs = import <nixpkgs> { inherit config; };

    in
      { $cwd_name = pkgs.haskellPackages.$cwd_name;
      }
    " > ./release.nix
}

# Work around the lack of "cabal new" plus add the nix stuff
hlix() {
    mcd $1
    cabal init
    nixup
}

# fzf settings
if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
fi
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

_gen_fzf_default_opts() {
  local base03="234"
  local base02="235"
  local base01="240"
  local base00="241"
  local base0="244"
  local base1="245"
  local base2="254"
  local base3="230"
  local yellow="136"
  local orange="166"
  local red="160"
  local magenta="125"
  local violet="61"
  local blue="33"
  local cyan="37"
  local green="64"
}

_gen_fzf_default_opts

source /usr/share/autojump/autojump.sh
fpath=(/usr/local/share/zsh-completions $fpath)
source /home/alexeyzab/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
