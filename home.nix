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

  ghidra-fix = pkgs.writeShellScriptBin "ghidra-fix" ''
    _JAVA_AWT_WM_NONREPARENTING=1 exec ghidra
  '';

  ncoq = pkgs.coq_8_15;
  ncoqPackages = pkgs.coqPackages_8_15;  
  stdpp-dev = ncoqPackages.callPackage
    ( { coq, stdenv, fetchFromGitLab }:
      stdenv.mkDerivation {
	      name = "coq${coq.coq-version}-stdpp";

	      src = fetchFromGitLab {
          domain = "gitlab.mpi-sws.org";
          owner = "iris";
          repo = "stdpp";
          rev = "master";
          sha256 = "Js5HuTXLtW2H9e/m1d+XW9z3+nAvpWFKYj9lYHGXteY=";
	      };

        preBuild = ''
          if [[ -f coq-lint.sh ]]
          then patchShebangs coq-lint.sh
          fi
        '';
        
	      buildInputs = with coq.ocamlPackages; [ ocaml camlp5 ];
	      propagatedBuildInputs = [ ncoq ];
	      enableParallelBuilding = true;

	      installFlags = [ "COQLIB=$(out)/lib/coq/${coq.coq-version}/" ];
      } ) { };

  iris-dev = ncoqPackages.callPackage
    ( { coq, stdenv, fetchFromGitLab }:
      stdenv.mkDerivation {
	      name = "coq${coq.coq-version}-iris";

	      src = fetchFromGitLab {
          domain = "gitlab.mpi-sws.org";
          owner = "iris";
          repo = "iris";
          rev = "53b2097438351ca9c9ed6cfece976e2a743859be";
          sha256 = "WjYJEK0LvPXez/8GXPMeiqkDJ1krtiLNAELTPNnfhfs=";
	      };

        preBuild = ''
          if [[ -f coq-lint.sh ]]
          then patchShebangs coq-lint.sh
          fi
        '';
        
	      buildInputs = with coq.ocamlPackages; [ ocaml camlp5 ];
	      propagatedBuildInputs = [ ncoq stdpp-dev ];
	      enableParallelBuilding = true;

	      installFlags = [ "COQLIB=$(out)/lib/coq/${coq.coq-version}/" ];
      } ) { };
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
      ./zathura.nix
    ];

  programs.home-manager.enable = true;
  programs.gpg.enable = true;
  programs.browserpass.enable = true;
  programs.opam = {
    enable = true;
    enableBashIntegration = true;
  };
  programs.zathura = {
    enable = true;
  };
  programs.go = {
    enable = true;
    # packages = {
    #   "golang.org/x/tools/goplst" = builtins.fetchGit "https://go.googlesource.com/tools";
    # };
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
    cutter
    davmail
    dino
    discord
    direnv
    dsniff
    dwarf-fortress
    element-desktop
    emacsopen
    firefox-wayland
    font-awesome
    freecad
    gajim
    ghidra
    ghidra-fix
    gimp
    gnome3.adwaita-icon-theme
    gopls
    gore
    go-ethereum
    gpa
    grim
    grub
    gtklp
    helvum
    jdk
    jetbrains.pycharm-community
    i2p
    icu
    imagemagick
    imv
    ispell
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
    patchelf
    pass-ext
    pavucontrol
    pcmanfm
    pidgin
    playerctl
    pkg-config
    protontricks
    proton-caller
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
    # unstable.coqPackages_8_15.iris
    # unstable.coqPackages_8_15.category-theory
    # unstable.coqPackages_8_15.stdpp
    # unstable.coqPackages_8_15.QuickChick
    # unstable.coq_8_15
    
    stdpp-dev
    iris-dev
    ncoq
    
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
    zoom-us    
  ];
  
  home.file.".davmail.properties".source = ./dotfiles/davmail.properties;
}
