{ config, lib, pkgs, ... }:

let
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

  my-python-packages = python-packages: with python-packages; [
    jupyter
    debugpy
    python-lsp-server
    pip
    pygments
    docutils
    alectryon
    scikit-learn
    matplotlib
    seaborn
    numpy
    scipy
    pandas
    requests
    beautifulsoup4
    scrapy
    web3
  ];
  python-with-my-packages = pkgs.python3.withPackages my-python-packages;

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

  pidgin-with-plugins = pkgs.pidgin.override {
    plugins = [ pkgs.pidgin-otr pkgs.pidgin-latex ];
  };

  agda = pkgs.agda.withPackages [ pkgs.agdaPackages.standard-library pkgs.agdaPackages.agda-categories pkgs.agdaPackages.cubical ];

  mattermost-fix = pkgs.writeShellScriptBin "mattermost-fix" ''
    exec mattermost-desktop -d ~/Mattermost
  '';

  emacsopen = pkgs.writeShellScriptBin "emacsopen" ''
    exec emacsclient -c
  '';

  ghidra-fix = pkgs.writeShellScriptBin "ghidra-fix" ''
    _JAVA_AWT_WM_NONREPARENTING=1 exec ghidra
  '';

  zoom-us-fix = pkgs.writeShellScriptBin "zoom-us-fix" ''
    unset XDG_SESSION_TYPE && ~/.nix-profile/bin/zoom-us
  '';

  ncoq = pkgs.coq_8_16;
  ncoqPackages = pkgs.coqPackages_8_16;
  # stdpp-dev = ncoqPackages.callPackage
  #   ( { stdenv, fetchFromGitLab }:
  #     stdenv.mkDerivation {
	#       name = "coq${ncoq.coq-version}-stdpp";

	#       src = fetchFromGitLab {
  #         domain = "gitlab.mpi-sws.org";
  #         owner = "iris";
  #         repo = "stdpp";
  #         rev = "coq-stdpp-1.8.0";
  #         sha256 = "VkIGBPHevHeHCo/Q759Q7y9WyhSF/4SMht4cOPuAXHU=";
	#       };

  #       preBuild = ''
  #         if [[ -f coq-lint.sh ]]
  #         then patchShebangs coq-lint.sh
  #         fi
  #       '';

	#       buildInputs = with ncoq.ocamlPackages; [ ocaml ];
	#       propagatedBuildInputs = [ ncoq ];
	#       enableParallelBuilding = true;

	#       installFlags = [ "COQLIB=$(out)/lib/coq/${ncoq.coq-version}/" ];
  #     } ) { };

  # iris-dev = ncoqPackages.callPackage
  #   ( { stdenv, fetchFromGitLab }:
  #     stdenv.mkDerivation {
	#       name = "coq${ncoq.coq-version}-iris";

	#       src = fetchFromGitLab {
  #         domain = "gitlab.mpi-sws.org";
  #         owner = "iris";
  #         repo = "iris";
  #         rev = "iris-4.0.0";
  #         sha256 = "Jc9TmgGvkiDaz9IOoExyeryU1E+Q37GN24NIM397/Gg=";
	#       };

  #       preBuild = ''
  #         if [[ -f coq-lint.sh ]]
  #         then patchShebangs coq-lint.sh
  #         fi
  #       '';

	#       buildInputs = with ncoq.ocamlPackages; [ ocaml ];
	#       propagatedBuildInputs = [ ncoq stdpp-dev ];
	#       enableParallelBuilding = true;

	#       installFlags = [ "COQLIB=$(out)/lib/coq/${ncoq.coq-version}/" ];
  #     } ) { };

  # hott-dev = ncoqPackages.callPackage
  #   ( { stdenv, fetchFromGitHub }:
  #     stdenv.mkDerivation {
	#       name = "coq${ncoq.coq-version}-hott";

	#       src = fetchFromGitHub {
  #         owner = "HoTT";
  #         repo = "Coq-HoTT";
  #         rev = "V8.15";
  #         sha256 = "JfeiRZVnrjn3SQ87y6dj9DWNwCzrkK3HBogeZARUn9g=";
	#       };

  #       preBuild = ''
  #         if [[ -f coq-lint.sh ]]
  #         then patchShebangs coq-lint.sh
  #         fi
  #       '';

	#       buildInputs = with ncoq.ocamlPackages; [ ocaml camlp5 ];
	#       propagatedBuildInputs = [ ncoq ];
	#       enableParallelBuilding = true;

	#       installFlags = [ "COQLIB=$(out)/lib/coq/${ncoq.coq-version}/" ];
  #     } ) { };
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
                                           "steam-run"
                                           "zoom"
                                           "discord"
                                           "spotify"
                                           "spotify-unwrapped"
                                           "dwarf-fortress"
                                         ];

  nixpkgs.config.permittedInsecurePackages = [
    "python3.10-poetry-1.2.2"
    "python3.10-certifi-2022.9.24"
  ];

  home.username = "kaptch";
  home.homeDirectory = "/home/kaptch";
  home.packages = with pkgs; [
    agda
    aircrack-ng
    alacritty
    anki-bin
    ardour
    authenticator
    baobab
    bemenu
    bettercap
    binutils
    bitwarden
    bitwarden-cli
    blender
    btop
    brightnessctl
    cabal-install
    cage
    cargo
    cargo-xbuild
    cataclysm-dda
    chromium
    clippy
    ncoqPackages.autosubst
    ncoqPackages.mathcomp-ssreflect
    ncoqPackages.equations
    ncoqPackages.category-theory
    ncoqPackages.metacoq
    ncoqPackages.metacoq-pcuic
    ncoqPackages.serapi
    ncoqPackages.iris
    ncoqPackages.stdpp
    crow-translate
    cutter
    davmail
    delta
    direnv
    discord
    docker-compose
    dsniff
    dwarf-fortress
    element-desktop
    emacs-all-the-icons-fonts
    emacsopen
    erlang
    erlang-ls
    erlfmt
    firefox-wayland
    font-awesome
    freecad
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
    gtk-layer-shell
    haskell-language-server
    helvum
    hicolor-icon-theme
    # hott-dev
    i2p
    icu
    imagemagick
    imv
    # iris-dev
    ispell
    jdk
    kanshi
    kismet
    # lean4pkg
    ledger-live-desktop
    libreoffice
    lispPackages.asdf
    lispPackages.quicklisp
    llvm
    llvm-manpages
    lutris
    macchanger
    mako
    mattermost
    mattermost-fix
    meson
    metasploit
    prismlauncher
    mkchromecast
    monero-gui
    mpv
    mutt
    mycrypto
    ncoq
    networkmanagerapplet
    nixopsUnstable
    nodePackages.npm
    nodejs
    nwg-launchers
    ocamlPackages.ocaml-lsp
    okular
    openssl
    ott
    pamixer
    papirus-icon-theme
    parted
    pass-ext
    patchelf
    pavucontrol
    pcmanfm
    pidgin-with-plugins
    pinentry-emacs
    pkg-config
    playerctl
    profanity
    proton-caller
    protontricks
    prusa-slicer
    pulseaudio
    pwgen
    python-with-my-packages
    qbittorrent
    qemu
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
    screenfetch
    signal-desktop
    slurp
    solc
    spotify
    # stdpp-dev
    steam
    spago
    sway-contrib.grimshot
    swayidle
    swaylock-fancy
    syncthingtray
    system-config-printer
    tdesktop
    tex
    ltex-ls
    thunderbird-wayland
    tmux
    (tor-browser-bundle-bin.override {
      useHardenedMalloc = false;
    })
    udisks
    unzip
    qt6.qtwayland
    vial
    vim
    virt-manager
    vlc
    waybar
    wdisplays
    wev
    weylus
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
    zoom-us-fix
  ];

  xdg.mimeApps.defaultApplications = {
    "application/x-extension-htm" = "firefox.desktop";
    "application/x-extension-html" = "firefox.desktop";
    "application/x-extension-shtml" = "firefox.desktop";
    "application/x-extension-xht" = "firefox.desktop";
    "application/x-extension-xhtml" = "firefox.desktop";
    "application/xhtml+xml" = "firefox.desktop";
    "text/html" = "firefox.desktop";
    "x-scheme-handler/chrome" = "firefox.desktop";
    "x-scheme-handler/http" = "firefox.desktop";
    "x-scheme-handler/https" = "firefox.desktop";
  };
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    extraConfig = {
      XDG_AUDIO_DIR = "$HOME/Audio";
      XDG_BOOKS_DIR = "$HOME/Books";
      XDG_EDU_DIR = "$HOME/Edu";
      XDG_GAMES_DIR = "$HOME/Games";
      XDG_MAILDIR_DIR = "$HOME/Maildir";
      XDG_ORG_DIR = "$HOME/Org";
      XDG_SCREENSHOTS_DIR = "$HOME/Pictures/Screenshots";
      XDG_WALLPAPER_DIR = "$HOME/Pictures/wallpapers";
      XDG_PROJECTS_DIR = "$HOME/Projects";
      XDG_SCRIPTS_DIR = "$HOME/Scripts";
      XDG_SECRETS_DIR = "$HOME/Secrets";
      XDG_MISC_DIR = "$HOME/Misc";
      XDG_TEMP_DIR = "$HOME/Temp";
    };
  };
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
  xdg.configFile."discord/settings.json" = {
    text = builtins.toJSON {
      SKIP_HOST_UPDATE = true;
      BACKGROUND_COLOR = "#202225";
      IS_MAXIMIZED = true;
      IS_MINIMIZED = false;
    };
  };
}
