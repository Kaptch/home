{ pkgs, config, ... }:
{
  services.udiskie = {
    enable = true;
  };

  services.emacs = {
    enable = true;
  };

  services.syncthing = {
    enable = true;
    # Broken
    # tray.enable = true;
    # tray.command = "syncthingtray";
  };

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
  };

  services.lorri.enable = true;

  services.gnome-keyring.enable = true;
  
  # Not found ???
  # services.swayidle = {
  #   enable = true;
  #   timeouts = [{ timeout = 60; command = "swaylock -fF"; }];
  #   events = [{ event = "before-sleep"; command = "swaylock"; }
  #             { event = "lock"; command = "lock"; }];
  # };

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

  systemd.user.sockets.dbus = {
    Unit = {
      Description = "D-Bus User Message Bus Socket";
    };
    Socket = {
      ListenStream = "%t/bus";
      ExecStartPost = "${pkgs.systemd}/bin/systemctl --user set-environment DBUS_SESSION_BUS_ADDRESS=unix:path=%t/bus";
    };
    Install = {
      WantedBy = [ "sockets.target" ];
      Also = [ "dbus.service" ];
    };
  };

  systemd.user.services.dbus = {
    Unit = {
      Description = "D-Bus User Message Bus";
      Requires = [ "dbus.socket" ];
    };
    Service = {
      ExecStart = "${pkgs.dbus}/bin/dbus-daemon --session --address=systemd: --nofork --nopidfile --systemd-activation";
      ExecReload = "${pkgs.dbus}/bin/dbus-send --print-reply --session --type=method_call --dest=org.freedesktop.DBus / org.freedesktop.DBus.ReloadConfig";
    };
    Install = {
      Also = [ "dbus.socket" ];
    };
  };

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

  # systemd.user.services.swayidle = {
  #   Unit.PartOf = [ "graphical-session.target" ];
  #   Install.WantedBy = [ "graphical-session.target" ];

  #   Service = {
  #     Environment = "PATH=${pkgs.bash}/bin:${config.wayland.windowManager.sway.package}/bin";
  #     ExecStart = ''
  #       ${pkgs.swayidle}/bin/swayidle -w \
  #           timeout 600 "${pkgs.swaylock-fancy}/bin/swaylock-fancy" \
  #           timeout 600 'swaymsg "output * dpms off"' \
  #               resume 'swaymsg "output * dpms on"' \
  #           before-sleep "${pkgs.swaylock-fancy}/bin/swaylock-fancy"
  #     '';
  #     Restart = "on-failure";
  #   };
  # };

  systemd.user.services.davmail = {    
    Unit = {
      Description = "Davmail gateway";
      Documentation = "man:davmail(1)";
      After = [ "network.target" ];      
    };
    Install = {
      WantedBy = [ "default.target" ];
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
}
