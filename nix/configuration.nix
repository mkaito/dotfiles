{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  hardware.cpu.amd.updateMicrocode = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Kernel parameters
  boot.kernelParams = [
    "amdgpu.audio=0"
  ];

  # Tmpfs stuff
  boot.tmpOnTmpfs = true;

  # Basic networking
  networking.hostName = "cryptbreaker";
  networking.interfaces.enp3s0.ipv4.addresses = [{
    address = "192.168.1.2"; prefixLength = 24;
  }];

  networking.defaultGateway = "192.168.1.1";
  networking.nameservers = [ "1.0.0.1" "8.8.8.8" "8.8.4.4" ];

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Atlantic/Canary";
  time.hardwareClockInLocalTime = true;

  # List packages installed in system profile. To search by name, run:
  environment.systemPackages = with pkgs; [
    vim pmount ntfs3g libelf
  ];

  environment.shells = [ pkgs.zsh ];

  fileSystems = [
    {
      mountPoint = "/home/chris/.cache";
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "nosuid" "nodev" "relatime" "size=16G" ];
    }
  ];

  # Support for Steam play (Proton/wine)'s esync feature:
  # See https://github.com/zfigura/wine/blob/esync/README.esync
  # https://github.com/ValveSoftware/Proton/blob/proton_3.7/PREREQS.md
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";
  security.pam.loginLimits = [{
    domain = "*";
    type = "hard";
    item = "nofile";
    value = "1048576";
  }];

  security.wrappers = {
    pmount.source = "${pkgs.pmount}/bin/pmount";
    pumount.source = "${pkgs.pmount}/bin/pumount";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.bash.enableCompletion = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable parallel builds
  nix.buildCores = 0;

  # Disable build sandboxing
  nix.useSandbox = false;

  # Closure pushing
  nix.trustedUsers = [ "root" "chris" ];

  nix.binaryCaches = [
    "https://disciplina.cachix.org"
    "https://cache.nixos.org/"
  ];

  nix.binaryCachePublicKeys = [
    "disciplina.cachix.org-1:zDeIFV5cu22v04EUuRITz/rYxpBCGKY82x0mIyEYjxE="
  ];

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    openFirewall = true;
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Multihead config
  services.xserver.xrandrHeads = [
    {
      output = "DisplayPort-0";
      monitorConfig = ''
        Option "DPMS" "false"
        Option "Position" "0 850"
      '';
      # primary = true;
    }
    {
      output = "DisplayPort-1";
      monitorConfig = ''
        Option "Rotate" "Right"
        Option "DPMS" "false"
        Option "Position" "3840 0"
      '';
    }
  ];

  services.xserver.dpi = 163;
  # fonts.fontconfig.dpi = 163;

  # Keyboard config
  services.xserver.layout = "us,es";
  services.xserver.xkbVariant = "basic,basic";
  services.xserver.xkbOptions = "compose:sclk,grp:lctrl_lwin_toggle";

  # Pointer devices
  services.xserver.inputClassSections = [
    ''
      Identifier "ZA11"
      MatchProduct "Kingsis Peripherals ZOWIE Gaming mouse"
      MatchIsPointer "on"
      Option "AccelSpeed" "-0.25"
      Option "AccelProfile" "flat"
    ''

    ''
      Identifier "HUGE"
      MatchProduct "ELECOM TrackBall Mouse HUGE TrackBall"
      MatchIsPointer "on"
      Option "AccelSpeed" "0.2"
      Option "AccelProfile" "flat"
    ''
  ];

  services.xserver.useGlamor = true;
  services.xserver.deviceSection = ''
    Option "TearFree" "true"
    Option "DRI" "3"
  '';

  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.libinput.enable = true;
  services.xserver.windowManager.default = "i3";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.autoLogin.enable = false;
  services.xserver.displayManager.sddm.autoLogin.relogin = false;
  services.xserver.displayManager.sddm.autoLogin.user = "chris";
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.desktopManager.default = "none";
  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.openbox.enable = true;

  services.xserver.windowManager.i3.extraSessionCommands = ''
    ## My eyes hurt!
    ${pkgs.redshift}/bin/redshift -l 28:-13 -t 5600:3400 -b 1.00:0.92 -m randr &!

    ## Numlock on
    ${pkgs.numlockx}/bin/numlockx on &!

    ## Clipboard sync
    ${pkgs.autocutsel}/bin/autocutsel -selection CLIPBOARD -fork &!
    ${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork &!

    ## Disable DPMS and prevent screen from blanking
    xset s off -dpms &!
  '';

  # Steam
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  networking.firewall.allowedTCPPorts = [
    27036 27031 # Steam Link
    33646 # Resilio
  ];

  networking.firewall.allowedTCPPortRanges = [
    { from = 56881; to = 56891; } # Deluge
  ];

  networking.firewall.allowedUDPPorts = [
    27031 27036
  ];

  services.udev.packages = [ pkgs.android-udev-rules ];
  services.udev.extraRules = ''
    # Teensy stuff for Ergodox EZ flashing
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

    # Steam Controller
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"
    KERNEL=="uinput", MODE="0660", GROUP="users", OPTIONS+="static_node=uinput"
    KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0666"
    KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0666"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0666"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0666"
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0666"
    KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0666"
    KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0666"
    KERNEL=="hidraw*", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="2009", MODE="0666"
    KERNEL=="hidraw*", KERNELS=="*057E:2009*", MODE="0666"
  '';

  # Misc
  services.bitlbee = {
    enable = true;
    plugins = [ pkgs.bitlbee-discord ];
  };

  services.plex = {
    enable = true;
    openFirewall = true;
  };
  users.users.plex.extraGroups = [ "users" ];

  hardware.u2f.enable = true;     # Yubikey hardware access
  services.emacs.enable = true;
  services.haveged.enable = true; # Entropy daemon
  services.pcscd.enable = true;   # Yubikey smartcard interface
  services.urxvtd.enable = true;

  virtualisation.virtualbox.host.enable = true;

  # Printing
  services.printing = {
    enable = true;
    logLevel = "debug";
    drivers = with pkgs; [ gutenprint postscript-lexmark ];
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  # Profile sync daemon
  services.psd.enable = true;

  # Resilio Sync
  services.resilio = {
    deviceName = "Hydra";
    enable = true;
    enableWebUI = true;
    httpListenAddr = "127.0.0.1";
    useUpnp = true;
    listeningPort = 33646;
  };
  users.extraUsers.rslsync.extraGroups = [ "users" ];

  # Uptimed
  services.uptimed.enable = true;

  # services.syncthing = {
  #   enable = true;
  #   group = "users";
  #   user = "chris";
  #   openDefaultPorts = true;
  #   dataDir = "/home/chris/.local/share/syncthing";
  # };

  # Pulseaudio
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.extraConfig = ''
    load-module module-udev-detect tsched=0
  '';

  ## Fonts
  fonts.fonts = with pkgs; [
    iosevka # Apparently, Iosevka Term is broken
    terminus_font
    fira-code
    fira-code-symbols

    ## Fonts for symbols/emoji/kana/etc
    symbola
    noto-fonts-emoji
    ipafont
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.android_sdk.accept_license = true;

  users.mutableUsers = false;
  users.extraUsers.root.hashedPassword = "$6$1LCCdqTmO8crgT3$bg0/wOwMEFN5Q.thnu7pT5dmeuEezKOefmEl9KgzwVD3G1tSBYvdoBq1Oja5QwE3y7Li2ooXmxwBkP9qqupst/";
  users.extraUsers.chris =
  {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "rslsync" "libvirtd" "sway" ];
    hashedPassword = "$6$p4f9brmRq1bmf9U$DC3sUJdRFBzJyL5ZN7zPccsw7nGQlQILykJrNw1tUwWNeBWNL5qxqurfp4bmKuLO.NNDA2Hr8.PfkH3pxDE1H0";
    shell = pkgs.zsh;
    packages = with pkgs; [
      ## Basics
      atool
      bashInteractive
      entr
      ethtool
      fasd
      fd
      file
      fzf
      git
      git-crypt
      gitAndTools.hub
      google-cloud-sdk
      gparted
      hdparm
      htop
      i7z
      inotify-tools
      iperf
      lm_sensors
      neovim
      p7zip
      parted
      pwgen
      ripgrep
      sshfs
      # tarsnap
      tmux
      tree
      unzip
      unrar
      wget
      xclip
      xsel
      zip
      getopt
      uptimed

      # Crypto
      cryptsetup
      gnupg
      gopass
      openssl
      pass
      yubikey-manager
      yubioath-desktop
      usbutils # lsusb for yubioath

      ## Desktop
      anki
      conky
      dunst
      galculator
      libnotify
      libreoffice-still
      pinentry
      ranger
      rofi
      rofi-pass
      xdotool
      xorg.xdpyinfo
      xorg.xev
      xorg.xkill
      xorg.xwininfo


      # Performance
      stress-ng
      cpufrequtils
      watch

      ## Web
      google-chrome
      deluge
      firefox-beta-bin
      # flashplayer
      # qutebrowser
      # ipfs

      ## Media
      beets
      calibre
      feh
      ffmpeg-full
      gimp
      zathura
      # llpp
      moc
      mpv
      obs-studio
      pandoc
      pavucontrol
      playerctl
      pngquant
      ponymix
      pulsemixer
      scrot
      maim
      spotify
      sxiv
      texlive.combined.scheme-full
      youtube-dl
      pngcrush

      ## Games
      # wineStaging
      wineWowPackages.staging
      (winetricks.override { wine = wineWowPackages.staging; })

      # (dwarf-fortress.override { enableDFHack = true; })

      # For steam tenfoot session
      # steam.override { nativeOnly = true; }
      steam
      steam-run-native
      tint2

      ## FFXIV launcher
      python36Full
      python36Packages.tkinter

      ## Chat/IM
      weechat
      slack
      discord
      oysttyer

      ## Spelling
      aspell
      aspellDicts.es
      aspellDicts.en

      ## Email
      afew
      alot
      isync
      msmtp
      notmuch
      notmuch-addrlookup
      w3m

      ## Development
      erlang
      tealdeer
      androidsdk
      bind
      clang
      direnv
      elixir
      gist
      gnumake
      jq
      jre
      mkpasswd
      mosh
      ncurses.dev # incocmp/tic/etc
      nmap
      shellcheck
      teensy-loader-cli
      vault
      jetbrains.idea-ultimate

      ## Nix tools
      nix-index
      nix-prefetch-scripts
      nix-prefetch-github
      cachix
      nixops
      patchelf
    ];
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03"; # Did you read the comment?

}
