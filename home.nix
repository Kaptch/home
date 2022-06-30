{ config, lib, pkgs, ... }:

let
  # Tex with dependent packages
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full
      gentium-tug pbox scalerel dashbox xifthen ifmtarg biblatex cleveref biber iftex xstring totpages environ;
  });

  # Extra python packages
  my-python-packages = python-packages: with python-packages; [
    z3
    jupyter
    pip
  ];  
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;

  keepass-with-plugins = pkgs.keepass.override {
    plugins = [ pkgs.keepass-keepassrpc pkgs.keepass-keeagent ];
  };

  gajim = pkgs.gajim.override { enableOmemoPluginDependencies = true; enableJingle = true; };

  pass = pkgs.pass.override { waylandSupport = true; };
  pass-ext = pass.withExtensions (ext: [ ext.pass-import
                                         ext.pass-update
                                         ext.pass-tomb]);

  dwarf-fortress = pkgs.dwarf-fortress-packages.dwarf-fortress-full.override {
    dfVersion = "0.47.05";
    theme = pkgs.dwarf-fortress-packages.themes.phoebus;
    enableIntro = false;
    enableFPS = true;
  };

  agda = pkgs.agda.withPackages [ pkgs.agdaPackages.standard-library pkgs.agdaPackages.agda-categories ];
  
  # Mattermost
  mattermost-fix = pkgs.writeShellScriptBin "mattermost-fix" ''
    exec mattermost-desktop -d ~/Mattermost
  '';

  emacsopen = pkgs.writeShellScriptBin "emacsopen" ''
    exec emacsclient -c
  '';
in

{
  imports =
    [
      ./bash.nix
      ./git.nix
      ./emacs.nix
      ./tmux.nix
      ./alacritty.nix
      ./obs.nix
      ./waybar.nix
      ./sway.nix
      ./services.nix
      ./wlogout.nix
    ];

  programs.home-manager.enable = true;
  programs.gpg.enable = true;
  programs.browserpass.enable = true;
  programs.opam = {
    enable = true;
    enableBashIntegration = true;
  };

  # nixpkgs.config.permittedInsecurePackages = [
  # ];
  
  nixpkgs.config.allowUnfreePredicate =
    pkg: builtins.elem (lib.getName pkg) [ "steam-original"
                                           "steam-runtime"
                                           "steam"
                                           "zoom"
                                           "discord"
                                           "spotify"
                                           "spotify-unwrapped"
                                           "via"
                                           "dwarf-fortress"
                                         ];  
  
  home.username = "kaptch";
  home.homeDirectory = "/home/kaptch";
  home.packages = with pkgs; [
    agda
    aircrack-ng
    alacritty
    ardour
    bemenu
    bettercap
    blender
    brave
    brightnessctl
    cabal-install
    cage
    cargo    
    cargo-xbuild
    cataclysm-dda
    chromium
    davmail
    dino
    discord
    dsniff
    dwarf-fortress
    element-desktop
    emacsopen
    firefox-wayland
    font-awesome
    freecad
    gajim
    gimp
    gnome3.adwaita-icon-theme
    go-ethereum
    gpa
    grim
    grub
    gtklp
    helvum
    jdk
    i2p
    imagemagick
    imv
    kanshi
    keepass-with-plugins
    kicad
    kismet
    ledger-live-desktop
    lutris
    macchanger
    mako
    mattermost
    mattermost-fix
    meson
    metasploit
    mkchromecast
    monero
    monero-gui
    mpv
    mutt
    mycrypto
    networkmanagerapplet
    nwg-launchers
    okular
    openssl
    pamixer
    parted
    pass-ext
    pavucontrol
    pcmanfm
    pidgin
    playerctl
    prusa-slicer
    pulseaudio
    pwgen
    python-with-my-packages
    qbittorrent
    qemu
    qmk
    qtpass
    radare2
    ranger
    reaverwps
    rustc
    rustfmt
    samba
    signal-desktop
    slurp
    spotify
    steam
    sway-contrib.grimshot
    swayidle
    swaylock-fancy
    syncthingtray
    system-config-printer
    tdesktop
    tex
    thunderbird-wayland
    tmux
    tor-browser-bundle-bin
    transmission
    transmission-remote-gtk
    udisks
    unstable.coqPackages_8_15.iris
    unstable.coqPackages_8_15.stdpp
    coqPackages_8_15.QuickChick
    unstable.coq_8_15    
    unzip
    via
    vial
    vim
    virt-manager
    vlc
    waybar
    wdisplays
    wev
    wf-recorder
    wget
    wireguard-tools
    wireshark
    wl-clipboard
    wlogout
    xdg-desktop-portal
    xdg-desktop-portal-wlr
    xdg-utils
    xz
    youtube-dl
    yubikey-manager
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    zathura
    zoom-us    
  ];
  
  home.file.".davmail.properties".source = ./dotfiles/davmail.properties;
}
