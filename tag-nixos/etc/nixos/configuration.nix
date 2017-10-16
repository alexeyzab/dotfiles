{ config, lib, pkgs, ... }:

with lib;


{
  ###########
  # General #
  ###########

  # The NixOS release version.
  system.stateVersion = "17.09";

  # Collect nix store garbage and optimise daily.
  nix.gc.automatic = true;
  nix.optimise.automatic = true;

  # Clear out /tmp after a fortnight and give all normal users a ~/tmp
  # cleaned out weekly.
  systemd.tmpfiles.rules = [ "d /tmp 1777 root root 14d" ] ++
    (let mkTmpDir = n: u: "d ${u.home}/tmp 0700 ${n} ${u.group} 7d";
     in mapAttrsToList mkTmpDir (filterAttrs (n: u: u.isNormalUser) config.users.extraUsers));

  users.mutableUsers = true;

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };

  networking = {
    hostName = "nixos-air";
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
    };
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    wireless.enable = false;
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    cleanTmpDir = true;
    extraModprobeConfig = ''
      options libata.force=noncq
      options snd_hda_intel index=0 model=intel-mac-auto id=PCH
      options snd_hda_intel index=1 model=intel-mac-auto id=HDMI
      options hid_apple fnmode=2
  '';
  };


  ##########
  # Locale #
  ##########

  # Locale
  i18n.defaultLocale = "en_US.UTF-8";

  # Timezone
  time.timeZone = "America/New_York";

  # Keyboard
  i18n.consoleKeyMap = "us";


  ############
  # Services #
  ############

  virtualisation.docker.enable = true;

  services = {
    # Only keep the last 500MiB of systemd journal.
    journald.extraConfig = "SystemMaxUse=500M";

    timesyncd.enable = true;

    gnome3.gnome-keyring.enable = true;
    postgresql = {
      enable = true;
      package = pkgs.postgresql94;
    };
    xserver = {
      layout = "us";
      autorun = true;
      libinput = {
        enable = true;
	middleEmulation = true;
	# naturalScrolling = true;
      };
      enable = true;
      desktopManager.xterm.enable = false;
      desktopManager.default = "none";
      displayManager.slim = {
        enable = true;
	defaultUser = "alexeyzab";
      };
    };
  };

  systemd.user.services."urxvtd" = {
    enable = true;
    description = "rxvt unicode daemon";
    wantedBy = [ "default.target" ];
    path = [ pkgs.rxvt_unicode ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd -q -o";
  };

  systemd.user.services."autocutsel" = {
    enable = true;
    description = "AutoCutSel";
    wantedBy = [ "default.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  };

  systemd.user.services."udiskie" = {
    enable = true;
    description = "udiskie to automount removable media";
    wantedBy = [ "default.target" ];
    path = with pkgs; [
      gnome3.defaultIconTheme
      gnome3.gnome_themes_standard
      pythonPackages.udiskie
    ];
    environment.XDG_DATA_DIRS="${pkgs.gnome3.defaultIconTheme}/share:${pkgs.gnome3.gnome_themes_standard}/share";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.python27Packages.udiskie}/bin/udiskie -a -t -n -F ";
  };


  ################
  # User account #
  ################

  users.extraUsers.alexeyzab = {
    isNormalUser = true;
    uid = 1000;
    group = "users";
    description = "Alexey Zabelin <hello@alexeyzabelin.com>";
    extraGroups = [
      "wheel"
      "docker"
      "messagebus"
      "systemd-journal"
      "disk"
      "audio"
      "video"
    ];
    createHome = true;
    home = "/home/alexeyzab";
    shell = pkgs.zsh;
  };


  ######################
  # Package management #
  ######################

  nixpkgs.config = {
    # Allow packages with non-free licenses.
    allowUnfree = true;

    # Enable chromium plugins.
    chromium = {
      enablePepperFlash = true;
      enablePepperPDF = true;
    };
  };

  # System-wide packages
  environment = {
    systemPackages = with pkgs; [
      autocutsel
      binutils
      blueman
      chromium
      curl
      fontconfig
      gitAndTools.gitFull
      openssl
      pavucontrol
      xsel
      zsh
      zsh-completions
      zsh-syntax-highlighting
    ];
  };

  fonts = {
    fontconfig.enable = true;
    fontconfig.defaultFonts.monospace = ["Iosevka"];
    enableFontDir = true;
    enableCoreFonts = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      inconsolata
      fantasque-sans-mono
      dejavu_fonts
      ubuntu_font_family
      terminus_font
      corefonts
      freefont_ttf
      font-droid
      google-fonts
      iosevka
      powerline-fonts
    ];
  };

  programs = {
    ssh.startAgent = false;
    slock.enable = true;
    tmux.enable = true;
    zsh = {
      enableCompletion = true;
      syntaxHighlighting = {
        enable = true;
	highlighters = ["main" "brackets" "pattern" "cursor" "root" "line"];
      };
    };
  };
}
