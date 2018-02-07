{ pkgs, ... }:

let
  xmonad = pkgs.xmonad-with-packages.override {
    packages = self: [
      self.xmonad-contrib
      self.xmonad-extras
      self.dbus
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
    cabal-install
    ctags
    deluge
    dmenu
    docker
    docker_compose
    dropbox
    emacs
    evince
    fd
    feh
    fzf
    ghc
    gitAndTools.hub
    gnome3.gnome-tweak-tool
    gnome3.nautilus
    gnumake
    gnupg
    gtk-engine-murrine
    haskellPackages.alex
    haskellPackages.codex
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
    pinentry
    postgresql
    psc-package
    purescript
    python27Packages.udiskie
    ranger
    rcm
    rcm
    ripgrep
    rofi
    rustfmt
    rustup
    rxvt_unicode
    sass
    slack
    slock
    spotify
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
      xrandr --output VIRTUAL1 --off --output DP3 --off --output eDP1 --off --output DP1 --off --output HDMI3 --off --output HDMI2 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI1 --off --output DP2 --off
      xinput --set-button-map "Logitech USB Trackball" 1 8 3 4 5 6 7 2 2
      xinput --set-prop "Logitech USB Trackball" "Evdev Wheel Emulation" 1
      xinput --set-prop "Logitech USB Trackball" "Evdev Wheel Emulation Button" 8
      xinput --set-prop "Logitech USB Trackball" "Evdev Wheel Emulation Axes" 6 7 4 5
      xinput --set-prop "Logitech USB Trackball" "Evdev Middle Button Emulation" 1
      xinput --set-prop "Logitech USB Trackball" "Evdev Middle Button Emulation Button" 9
      xrdb ~/.Xresources
      xset s off -dpms
      xsetroot -cursor_name left_ptr
    '';
    initExtra = ''
      source ~/.config/zsh/.zshenv
      gpg-connect-agent /bye
      export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      dropbox &
      nm-applet &
      pasystray &
      blueman-manager &
      dunst &
      feh --bg-scale ~/.local/share/city_wallpaper.jpg &
      alacritty &
      emacs &
      chromium-browser &
    '';
  };
}
