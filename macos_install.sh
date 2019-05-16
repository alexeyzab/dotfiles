#!/bin/bash
brew update
brew tap caskroom/cask
brew cask install dropbox firefox-developer-edition google-chrome emacs karabiner-elements alacritty
brew install git zsh gnupg tmux htop reattach-to-user-namespace

# zsh configs
cd /Users/alexeyzab
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
git clone git://github.com/zsh-users/zsh-completions.git

# rcm
brew tap thoughtbot/formulae
brew install rcm

# dotfiles
# git clone https://github.com/alexeyzab/dotfiles ~/.dotfiles
cd /Users/alexeyzab/.dotfiles
rcup -t emacs
rcup -t stack
rcup -t git
rcup -t gpg
rcup -t gui
rcup -t haskell
rcup -t tmux
rcup -t zsh
# cp ./tag-zsh/config/zsh/.aliases ~/.config/zsh/
# cp ./tag-zsh/config/zsh/.zshenv ~/.config/zsh/
# cp ./tag-zsh/config/zsh/.zshrc ~/.config/zsh/

# tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# emacs setup
cd ~/.emacs.d
git clone https://github.com/hrs/sensible-defaults.el sensible-defaults
git clone https://github.com/tripleee/my-site-start
git clone https://github.com/jwiegley/use-package

# stack
curl -sSL https://get.haskellstack.org/ | sh

# rustup
curl https://sh.rustup.rs -sSf | sh
source $HOME/.cargo/env
rustup completions zsh > ~/.zfunc/_rustup
rustup update
rustup install nightly
rustup default nightly

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install --all

# fd
cargo install fd-find

# ripgrep
cargo install ripgrep
