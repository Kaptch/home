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
    debugpy
    python-lsp-server
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
    #   "https://pkg.go.dev/github.com/go-delve/delve" = pkgs.fetchFromGitHub {
    #       owner = "go-delve";
    #       repo = "delve";
    #       rev = "3fb2d49829187388d75c70fa46a18e0361a0292f";
    #       sha256 = "paNr9aiRG6NP6DIGUojl7VPPPMTeJRpDW8ThDNOQhWM=";
	  #     };
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
    clippy
    cutter
    davmail
    dino
    direnv
    discord
    dsniff
    dwarf-fortress
    element-desktop
    emacsopen
    erlang
    erlang-ls
    erlfmt
    firefox-wayland
    font-awesome
    freecad
    gajim
    ghidra
    ghidra-fix
    gimp
    gnome3.adwaita-icon-theme
    go-ethereum
    gopls
    gore
    gpa
    grim
    grub
    gtklp
    haskell-language-server
    helvum
    i2p
    icu
    imagemagick
    imv
    iris-dev
    ispell
    jdk
    jetbrains.pycharm-community
    kanshi
    keepass-with-plugins
    kicad
    kismet
    ledger-live-desktop
    lispPackages.asdf
    lispPackages.quicklisp
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
    ncoq    
    networkmanagerapplet
    nwg-launchers
    okular
    openssl
    pamixer
    parted
    pass-ext
    patchelf
    pavucontrol
    pcmanfm
    pidgin
    pkg-config
    playerctl
    proton-caller
    protontricks
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
    rebar3
    rust-analyzer
    rustc
    rustfmt
    samba
    sbcl
    signal-desktop
    slurp
    spotify
    stdpp-dev
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
