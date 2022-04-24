{ config, lib, pkgs, unstable, nixpkgs, nur, ... }:

let
  # Start sway script
  start-sway = pkgs.writeShellScriptBin "start-sway" ''
    # first import environment variables from the login manager
    systemctl --user import-environment
    # then start the service
    exec systemctl --user start sway.service
  '';

  # Tex with dependent packages
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-medium
      gentium-tug pbox scalerel dashbox xifthen ifmtarg biblatex cleveref biber;
  });

  # Extra python packages
  my-python-packages = python-packages: with python-packages; [
    z3
    jupyter
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

  # Mattermost
  mattermost-with-datadir = pkgs.writeShellScriptBin "mattermost-with-datadir" ''
    exec mattermost-desktop -d ~/Mattermost
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
    wget
    unzip
    udisks
    zathura
    zoom-us
    imagemagick
    vlc
    firefox-wayland
    mutt
    parted
    syncthingtray
    xz
    agda
    rustup
    opam
    discord
    pass-ext
    pwgen
    qtpass
    steam
    tdesktop
    signal-desktop
    gajim
    thunderbird-wayland
    alacritty
    tmux
    gimp
    swaylock-fancy
    swayidle
    wl-clipboard
    mako
    waybar
    kanshi
    bemenu
    grim
    wdisplays
    element-desktop
    wireguard-tools
    start-sway
    imv
    mpv
    slurp
    pidgin
    brightnessctl
    font-awesome
    wev
    sway-contrib.grimshot
    ranger
    pcmanfm
    ardour
    nwg-launchers
    pavucontrol
    pamixer
    spotify
    mattermost-desktop
    mattermost-with-datadir
    samba
    xdg-utils
    blender
    transmission
    transmission-remote-gtk
    tex
    playerctl
    pulseaudio
    okular
    via
    qmk
    vial
    gtklp
    wlogout
    xdg-desktop-portal
    xdg-desktop-portal-wlr
    wf-recorder
    helvum    
    lutris
    mkchromecast
    davmail    
    system-config-printer
    # nextcloud-client
    networkmanagerapplet
    tor-browser-bundle-bin
    i2p
    binutils.bintools # TODO: remove
    keepass-with-plugins
    yubikey-manager
    yubikey-personalization
    yubikey-manager-qt
    yubikey-personalization-gui
    yubioath-desktop
    gnome3.adwaita-icon-theme
    coqPackages_8_14.stdpp
    coqPackages_8_14.iris
    coq_8_14
    python-with-my-packages
    (dwarf-fortress-packages.dwarf-fortress-full.override {
      dfVersion = "0.47.05";
      theme = dwarf-fortress-packages.themes.phoebus;
      enableIntro = false;
      enableFPS = true;
    })
    cataclysm-dda
  ];
  
  home.file.".davmail.properties".source = ./dotfiles/davmail.properties;
  # home.file.".config/oguri/config".source = ./dotfiles/oguri;
  home.file.".config/kanshi/config".source = ./dotfiles/kanshi;
}
