{ pkgs, ... }:

let
  xmonad = pkgs.xmonad-with-packages.override {
    packages = self: [
      self.xmonad-contrib
      self.xmonad-extras
    ];
  };
in
{
  home.packages = with pkgs; [
    # Waiting for this to be updated
    # rescuetime
    arandr
    arc-theme
    aspell
    bleachbit
    cabal-install
    cabal2nix
    ctags
    deluge
    dmenu
    docker
    dropbox
    emacs
    evince
    feh
    fzf
    ghc
    gitAndTools.hub
    gnome3.gnome-tweak-tool
    gnome3.nautilus
    gnumake
    gnupg
    haskellPackages.codex
    haskellPackages.hasktags
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.stylish-haskell
    haskellPackages.styx
    haskellPackages.threadscope
    haskellPackages.weeder
    haskellPackages.xmonad
    htop
    lxappearance
    mirage
    mpv
    ncdu
    networkmanagerapplet
    nix-prefetch-git
    nix-repl
    nix-zsh-completions
    nodePackages.gulp
    nodePackages.npm
    nodejs
    pinentry
    postgresql
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
    skype
    slock
    spotify
    stack
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

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  services.network-manager-applet.enable = true;

  xsession = {
    enable = true;
    windowManager.command = "${xmonad}/bin/xmonad";
    initExtra = ''
      xrandr --output HDMI1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output eDP1 --off
      xinput --set-button-map "Logitech USB Trackball" 1 8 3 4 5 6 7 2 2
      xinput --set-prop "Logitech USB Trackball" "Evdev Wheel Emulation" 1
      xinput --set-prop "Logitech USB Trackball" "Evdev Wheel Emulation Button" 8
      xinput --set-prop "Logitech USB Trackball" "Evdev Wheel Emulation Axes" 6 7 4 5
      xinput --set-prop "Logitech USB Trackball" "Evdev Middle Button Emulation" 1
      xinput --set-prop "Logitech USB Trackball" "Evdev Middle Button Emulation Button" 9
      xrdb ~/.Xresources
      xset s off -dpms
      xsetroot -cursor_name left_ptr
      source ~/.config/zsh/.zshenv
      rescuetime &
      dropbox &
      nm-applet &
      feh --bg-scale ~/.local/share/city_wallpaper.jpg
    '';
  };
}
