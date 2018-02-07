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
    trackpoint = {
      enable = true;
      emulateWheel = true;
    };
    bumblebee = {
      enable = true;
      connectDisplay = true;
    };
    bluetooth = {
      enable = true;
    };
  };

  sound = {
    mediaKeys.enable = true;
  };

  networking = {
    hostName = "nixos-thinkpad";
    networkmanager = {
      enable = true;
      insertNameservers = [ "8.8.8.8" "8.8.4.4" ];
    };
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    wireless.enable = false;
  };

  boot = {
    # Switching to the latest Linux Kernel.
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    cleanTmpDir = true;
  };


  ##########
  # Locale #
  ##########

  # Locale
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.supportedLocales = ["all"];

  # Timezone
  time.timeZone = "America/New_York";

  # Keyboard
  i18n.consoleKeyMap = "us";


  ############
  # Services #
  ############

  virtualisation.docker.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.libvirtd.enable = true;

  services = {
    #virtualbox.host = {
    #  enable = true;
    #  enableHardening = false;
    #};
    # Only keep the last 500MiB of systemd journal.
    journald.extraConfig = "SystemMaxUse=500M";

    timesyncd.enable = true;
    tlp.enable = true;
    thermald.enable = true;
    # teamviewer.enable = true;

    gnome3.gnome-keyring.enable = true;
    gnome3.at-spi2-core.enable = true;

    postgresql = {
      enable = true;
      package = pkgs.postgresql94;
    };
    mysql = {
      enable = true;
      dataDir = "/var/db/mysql";
      package = pkgs.mysql57;
    };
    xserver = {
      layout = "us";
      autorun = true;
      xkbVariant = "mac";
      #xkbOptions = "grp:alt_space_toggle, ctrl:swapcaps";
      synaptics.enable = false;
      libinput = {
        enable = true;
	middleEmulation = true;
	buttonMapping = "1 2 3";
	clickMethod = "buttonareas";
	# naturalScrolling = true;
      };
      enable = true;
      desktopManager.xterm.enable = false;
      desktopManager.default = "none";
      displayManager.slim = {
        enable = true;
	defaultUser = "alexeyzab";
      };
      videoDrivers = [ "intel" "vesa" ];
    };
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
      "libvirtd"
      "kvm"
      "vboxusers"
    ];
    createHome = true;
    home = "/home/alexeyzab";
    shell = pkgs.zsh;
  };
  users.extraGroups.vboxusers.members = ["alexeyzab"];
  users.extraGroups.networkmanager.members = ["root"];


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

    google-chrome = {
      enablePepperFlash = true;
      enablePepperPDF = true;
      enableWideVine = true;
    };
  };

  # System-wide packages
  environment = {
    variables = {
      GOROOT = [ "${pkgs.go.out}/share/go" ];
    };
    systemPackages = with pkgs; [
      #teamviewer
      #virtualbox
      acpi
      linuxPackages.acpi_call
      autocutsel
      binutils
      chromium
      google-chrome
      curl
      networkmanagerapplet
      networkmanager_openvpn
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
    fontconfig.defaultFonts.monospace = ["Fantasque Sans Mono"];
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
    gnupg.agent.enable = true;
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
