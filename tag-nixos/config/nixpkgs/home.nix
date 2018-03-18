{ pkgs, ... }:

let
  xmonad = pkgs.xmonad-with-packages.override {
    packages = self: [
      self.xmonad-contrib
      self.xmonad-extras
      self.utf8-string
      self.taffybar
    ];
  };
in
{
  home.packages = with pkgs; [
    taffybar
    alacritty
    go
    arandr
    arc-icon-theme
    arc-theme
    aspell
    bleachbit
    ctags
    deluge
    dmenu
    docker
    docker_compose
    emacs
    evince
    fd
    feh
    fzf
    gitAndTools.hub
    gnome3.gnome-tweak-tool
    gnome3.nautilus
    gnumake
    gnupg
    gtk-engine-murrine
    haskellPackages.alex
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.stylish-haskell
    haskellPackages.styx
    haskellPackages.threadscope
    haskellPackages.weeder
    haskellPackages.xmonad
    htop
    icdiff
    libnotify
    lxappearance
    mirage
    mpv
    mysql-workbench
    ncdu
    nix-prefetch-git
    nix-repl
    nix-zsh-completions
    nodePackages.gulp
    nodePackages.npm
    nodejs
    postgresql
    purescript
    python27Packages.udiskie
    ranger
    rcm
    rcm
    ripgrep
    rofi
    rustup
    rxvt_unicode
    sass
    slack
    slock
    stalonetray
    texlive.combined.scheme-full
    tldr
    tmux
    tree
    unzip
    urlview
    urxvt_autocomplete_all_the_things
    urxvt_perls
    vim
    zathura
  ];

  programs.home-manager.enable = true;
  programs.home-manager.path = "https://github.com/rycee/home-manager/archive/master.tar.gz";

  services.taffybar = {
    enable = true;
  };

  gtk.gtk3.extraconfig = {
    gtk-key-theme-name = "Emacs";
  };

  services.network-manager-applet.enable = true;

  xsession = {
    enable = true;
    windowManager.command = "${xmonad}/bin/xmonad";
    profileExtra = ''
      xrandr --output DVI-D-0 --off --output HDMI-0 --off --output DP-5 --off --output DP-4 --off --output DP-3 --off --output DP-2 --primary --mode 1920x1080 -r 144 --pos 0x448 --rotate normal --output DP-1 --off --output DP-0 --mode 1920x1080 -r 144 --pos 1920x0 --rotate left
      xrdb ~/.Xresources
      xmodmap ~/.Xmodmap
      xset s off -dpms
      xsetroot -cursor_name left_ptr
    '';
    initExtra = ''
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      dropbox &
      nm-applet &
      pasystray &
      dunst &
      feh --bg-scale ~/.local/share/nix-wallpaper-simple-dark-gray.png &
      alacritty &
      emacs &
      chromium-browser &
      slack &
    '';
  };
}
