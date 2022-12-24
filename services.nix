{ pkgs, config, lib, ... }:
{
  services.udiskie = {
    enable = true;
    tray = "always";
  };

  services.emacs = {
    enable = true;
  };

  services.poweralertd = {
    enable = true;
  };

  services.syncthing = {
    enable = true;
    # tray = {
    #   enable = true;
    #   command = "syncthingtray --wait";
    #   package = pkgs.syncthingtray;
    # };
  };

  # systemd.user.services.syncthingtray.Service.ExecStart = lib.mkForce "${pkgs.bash}/bin/bash -c '${pkgs.coreutils}/bin/sleep 5; ${pkgs.syncthingtray-minimal}/bin/syncthingtray'";

  programs.ssh = {
    enable = true;
    forwardAgent = true;
    extraConfig = ''
    Host gpgtunnel
        HostName localhost
        StreamLocalBindUnlink yes
        Port 10022
        User kaptch
        RemoteForward /run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra
  '';
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "curses";
    enableExtraSocket = true;
    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };

  services.lorri.enable = true;

  services.imapnotify.enable = true;

  services.gnome-keyring.enable = true;

  services.swayidle = {
    enable = true;
    timeouts = [
      { timeout = 1200; command = "${pkgs.swaylock-fancy}/bin/swaylock-fancy"; }
      { timeout = 2400; command = "swaymsg 'output * dpms off'"; resumeCommand = "swaymsg 'output * dpms on'"; }
    ];
    events = [
      { event = "before-sleep"; command = "${pkgs.swaylock-fancy}/bin/swaylock-fancy"; }
    ];
  };

  services.kanshi = {
    enable = true;
    profiles = {
      undocked = {
        outputs = [
          {
            criteria = "eDP-1";
          }
        ];
      };
      docked = {
        outputs = [
          {
            criteria = "eDP-1";
          }
          {
            criteria = "HDMI-A-1";
          }
        ];
      };
    };
  };

  # services.gammastep = {
  #   enable = true;
  #   dawnTime = "6:00-7:45";
  #   duskTime = "18:35-20:15";
  #   # tray = true;
  #   settings = {
  #     general = {
  #       adjustment-method = "wayland";
  #     };
  #   };
  # };

  programs.mako = {
    enable = true;
    layer = "overlay";
    font = "IBM Plex 13";
    width = 500;
    height = 80;
    defaultTimeout = 10000;
    maxVisible = 10;
    backgroundColor = "#000000AA";
    textColor = "#FFFFFF";
    borderColor = "#444444AA";
    progressColor = "over #11AA11";
    maxIconSize = 24;
  };

  systemd.user.services.davmail = {
    Unit = {
      Description = "Davmail gateway";
      Documentation = "man:davmail(1)";
      After = [ "network.target" ];
    };
    Install = {
      WantedBy = [ "multi-user.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.davmail}/bin/davmail";
      RestartSec = 5;
      Restart = "always";
      LogsDirectory = "davmail";
    };
  };

  systemd.user.services.nm-applet = {
    Unit = {
      Description = "Network Manager Applet";
      BindsTo = [ "sway-session.target" ];
      After = [ "sway-session.target" ];
      ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
    };
    Service = {
      ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet --sm-disable --indicator";
    };
    Install = {
      WantedBy = [ "sway-session.target" ];
    };
  };

  # Broken for now
  # systemd.user.services.crow = {
  #   Unit = {
  #     Description = "Crow Translator";
  #     BindsTo = [ "sway-session.target" ];
  #     After = [ "sway-session.target" ];
  #   };
  #   Service = {
  #     ExecStart = "${pkgs.crow-translate}/bin/crow";
  #   };
  #   Install = {
  #     WantedBy = [ "sway-session.target" ];
  #   };
  # };
}
