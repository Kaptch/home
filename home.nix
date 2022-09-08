{ config, lib, pkgs, ... }:

let
  # Tex with dependent packages
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-full
      gentium-tug
      pbox
      scalerel
      dashbox
      xifthen
      ifmtarg
      biblatex
      cleveref
      biber
      iftex
      xstring
      totpages
      environ;
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

  # keepass-with-plugins = pkgs.keepass.override {
  #   plugins = [ pkgs.keepass-keepassrpc pkgs.keepass-keeagent ];
  # };

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
          rev = "coq-stdpp-1.8.0";
          sha256 = "VkIGBPHevHeHCo/Q759Q7y9WyhSF/4SMht4cOPuAXHU=";
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
          rev = "iris-4.0.0";
          sha256 = "Jc9TmgGvkiDaz9IOoExyeryU1E+Q37GN24NIM397/Gg=";
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
    package = pkgs.go_1_18;
  };
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };
  programs.mbsync = {
    enable = true;
    extraConfig = "Sync Pull\n";
  };
  programs.msmtp = {
    enable = true;
  };
  programs.mu.enable = true;
  programs.neovim.enable = true;
  programs.neomutt = {
    enable = true;
    sidebar.enable = true;
    editor = "emacsclient -c";
  };

  accounts.email = {
    accounts = {
      kaptch = {
        address = "kaptch@gmail.com";
        gpg = {
          key = "E28D035B17973498838DF2FC2468D8CD84976F6E";
          signByDefault = true;
        };
        imap = {
          host = "imap.gmail.com";
        };
        mbsync = {
          enable = true;
          create = "maildir";
          extraConfig.channel = {
            MaxMessages = 1000;
            ExpireUnread = "yes";
          };
        };
        msmtp = {
          enable = true;
          extraConfig = {
            auth = "on";
            tls_starttls = "on";
            logfile = "~/.msmtp.log";
          };
        };
        mu.enable = true;
        neomutt.enable = true;
        primary = true;
        realName = "Sergei Stepanenko";
        signature = {
          text = ''
          Kind regards/Med venlig hilsen,
          Sergei
        '';
          showSignature = "append";
        };
        # INSIDE_EMACS='YES'
        passwordCommand = "echo $(INSIDE_EMACS='YES' gpg2 -q --for-your-eyes-only --no-tty -d /home/kaptch/.authinfo.gpg 2> /dev/null | awk '/machine gmail.com login kaptch@gmail.com/ {print $NF}' 2> /dev/null)";
        smtp = {
          host = "smtp.gmail.com";
          port = 587;
        };
        userName = "kaptch@gmail.com";
      };
      au = {
        address = "sergei.stepanenko@cs.au.dk";
        gpg = {
          key = "E28D035B17973498838DF2FC2468D8CD84976F6E";
          signByDefault = true;
        };
        imap = {
          host = "127.0.0.1";
          port = 1143;
          tls.enable = false;
        };
        mbsync = {
          enable = true;
          create = "maildir";
          extraConfig.account = {
            AuthMechs = "LOGIN";
          };
          extraConfig.channel = {
            MaxMessages = 1000;
            ExpireUnread = "yes";
          };
        };
        msmtp = {
          enable = true;
          extraConfig = {
            auth = "login";
            tls = "off";
            logfile = "~/.msmtp.log";
          };
        };
        mu.enable = true;
        neomutt.enable = true;
        primary = false;
        realName = "Sergei Stepanenko";
        signature = {
          text = ''
          Kind regards/Med venlig hilsen,
          Sergei
        '';
          showSignature = "append";
        };
        # INSIDE_EMACS='YES'
        passwordCommand = "echo $(INSIDE_EMACS='YES' gpg2 -q --for-your-eyes-only --no-tty -d /home/kaptch/.authinfo.gpg 2> /dev/null | awk '/machine au login au671308@uni.au.dk/ {print $NF}' 2> /dev/null)";
        smtp = {
          host = "localhost";
          port = 1025;
        };
        userName = "au671308@uni.au.dk";
      };
    };
  };

  # nixpkgs.config.permittedInsecurePackages = [
  # ];

  # nixpkgs.overlays = [
  #   (self: super: {
  #     hiedb = pkgs.haskell.lib.dontCheck super.hiedb;
  #   })
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
    authenticator
    bemenu
    bettercap
    binutils
    bitwarden
    bitwarden-cli
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
    crow-translate
    cutter
    davmail
    dino
    direnv
    discord
    docker-compose
    dsniff
    dwarf-fortress
    element-desktop
    emacsopen
    emacs-all-the-icons-fonts
    erlang
    erlang-ls
    erlfmt
    fbreader
    firefox-wayland
    font-awesome
    freecad
    libreoffice
    gajim
    gh
    ghidra
    ghidra-fix
    gimp
    gnome3.adwaita-icon-theme
    gnumake
    go-ethereum
    gopls
    gore
    gpa
    grim
    grub
    gtklp
    # haskell-language-server
    helvum
    hicolor-icon-theme
    i2p
    icu
    imagemagick
    imv
    iris-dev
    ispell
    javaPackages.openjfx17
    jdk
    jetbrains.pycharm-community
    kanshi
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
    nixopsUnstable
    nwg-launchers
    ocamlPackages.ocaml-lsp
    okular
    openssl
    pamixer
    parted
    papirus-icon-theme
    pass-ext
    patchelf
    pavucontrol
    pcmanfm
    pidgin
    pinentry-emacs
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
  xdg.configFile."swaylock/config" = {
    text = lib.concatStrings (lib.mapAttrsToList (n: v:
      if v == false then
        ""
      else
        (if v == true then n else n + "=" + builtins.toString v) + "\n")
      {
        color = "#000000";
        font-size = 24;
        indicator-idle-visible = false;
        indicator-radius = 100;
        line-color = "12175c";
        show-failed-attempts = true;
      });
  };
}
