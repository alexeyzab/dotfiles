#!/bin/bash
sudo apt install git curl zsh tree rofi autoconf autogen pasystray arc-theme aspell exuberant-ctags deluge feh evince pinentry-gnome3 pass gnome-tweak-tool icdiff lxappearance htop mirage ncdu postgresql ranger suckless-tools tmux tldr dunst blueman unzip urlview zathura autocutsel autojump nm-tray pavucontrol openssl xsel  fonts-fantasque-sans cmake libfreetype6-dev libasound2 libasound2-dev libdbusmenu-glib-dev libdbusmenu-gtk3-dev arandr libfontconfig1-dev xclip emacs chromium-browser libgirepository1.0-dev libwebkit2gtk-4.0-dev libgtksourceview-3.0-dev -y
sudo apt-key adv --keyserver pgp.mit.edu --recv-keys 1C61A2656FB57B7E4DE0F4C1FC918B335044912E
sudo apt update
sudo apt install slack-desktop
sudo apt install dropbox python3-gpg
chsh -s /bin/zsh
mkdir -p ~/.local/bin
mkdir -p ~/.cargo/bin

# autojump

# hub
cd /home/alexeyzab/.local/bin
wget https://github.com/github/hub/releases/download/v2.3.0-pre10/hub-linux-amd64-2.3.0-pre10.tgz
cp hub-linux-amd64-2.3.0-pre10/bin/hub /home/alexeyzab/.local/bin/
chmod +x ./hub
rm hub-linux-amd64-2.3.0-pre10.tgz

# docker
sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo apt-key fingerprint 0EBFCD88
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu artful stable"
sudo apt update
sudo apt install docker-ce
sudo curl -L https://github.com/docker/compose/releases/download/1.21.2/docker-compose-$(uname -s)-$(uname -m) -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker

# zsh configs
cd /home/alexeyzab
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git
git clone git://github.com/zsh-users/zsh-completions.git

# rcm
sudo add-apt-repository ppa:martin-frost/thoughtbot-rcm -y
sudo apt update
sudo apt install rcm

# dotfiles
git clone https://github.com/alexeyzab/dotfiles ~/.dotfiles
cd /home/alexeyzab/.dotfiles
rcup -t emacs
rcup -t stack
rcup -t git
rcup -t gpg
rcup -t gui
rcup -t haskell
rcup -t tmux
rcup -t zsh
cp ./tag-zsh/config/zsh/.aliases ~/.config/zsh/
cp ./tag-zsh/config/zsh/.zshenv ~/.config/zsh/
cp ./tag-zsh/config/zsh/.zshrc ~/.config/zsh/

# tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# emacs setup
cd ~/.emacs.d
git clone https://github.com/hrs/sensible-defaults.el sensible-defaults
git clone https://github.com/tripleee/my-site-start
git clone https://github.com/jwiegley/use-package

# stack
curl -sSL https://get.haskellstack.org/ | sh

# taffybar and xmonad setup
cd /home/alexeyzab/.xmonad
stack install status-notifier-item
git clone https://github.com/taffybar/taffybar
/home/alexeyzab/.xmonad/build.sh

# greenclip
cd /home/alexeyzab/.local/bin
curl -O https://github.com/erebe/greenclip/releases/download/3.0-beta/greenclip
chmod +x ./greenclip

# rustup
curl https://sh.rustup.rs -sSf | sh
source $HOME/.cargo/env

# alacritty
cd /home/alexeyzab
git clone https://github.com/jwilm/alacritty.git
cd alacritty
rustup override set stable
rustup update stable
cargo build --release
cp target/release/alacritty ~/.cargo/bin
cp Alacritty.desktop ~/.local/share/applications
sudo cp alacritty-completions.zsh /usr/share/zsh/functions/Completion/X/_alacritty

# fd
cargo install fd-find

# ripgrep
cargo install ripgrep

# fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install --all

# arc-theme icons
cd /home/alexeyzab
sudo add-apt-repository ppa:moka/daily
sudo apt update
sudo apt-get install moka-icon-theme faba-icon-theme faba-mono-icons
git clone https://github.com/horst3180/arc-icon-theme --depth 1 && cd arc-icon-theme
./autogen.sh --prefix=/usr
sudo make install
