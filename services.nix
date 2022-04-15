{ pkgs, config, ... }:
{
  services.udiskie = {
    enable = true;
  };

  services.emacs = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gtk2";
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

  systemd.user.services.sway = {
    Unit = {
      Description = "Sway - Wayland window manager";
      Documentation = [ "man:sway(5)" ];
      BindsTo = [ "graphical-session.target" ];
      Wants = [ "graphical-session-pre.target" ];
      After = [ "graphical-session-pre.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.sway}/bin/sway";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  systemd.user.services.mako = {
    Unit = {
      Description = "Mako notification daemon";
      PartOf = [ "graphical-session.target" ];
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
    Service = {
      Type = "dbus";
      BusName = "org.freedesktop.Notifications";
      ExecStart = "${pkgs.mako}/bin/mako";
      RestartSec = 5;
      Restart = "always";
    };
  };

  systemd.user.services.kanshi = {
    Unit = {
      Description = "Kanshi dynamic display configuration";
      PartOf = [ "graphical-session.target" ];
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.kanshi}/bin/kanshi";
      RestartSec = 5;
      Restart = "always";
    };
  };

  systemd.user.services.swayidle = {
    Unit.PartOf = [ "graphical-session.target" ];
    Install.WantedBy = [ "graphical-session.target" ];

    Service = {
      Environment = "PATH=${pkgs.bash}/bin:${config.wayland.windowManager.sway.package}/bin";
      ExecStart = ''
        ${pkgs.swayidle}/bin/swayidle -w \
            timeout 600 "${pkgs.swaylock-fancy}/bin/swaylock-fancy" \
            timeout 600 'swaymsg "output * dpms off"' \
                resume 'swaymsg "output * dpms on"' \
            before-sleep "${pkgs.swaylock-fancy}/bin/swaylock-fancy"
      '';
      Restart = "on-failure";
    };
  };

  # systemd.user.services.oguri = {
  #   Unit = {
  #     Description = "A very nice animated wallpaper daemon for Wayland compositors";
  #     PartOf = [ "graphical-session.target" ];
  #   };
  #   Install = {
  #     WantedBy = [ "graphical-session.target" ];
  #   };
  #   Service = {
  #     Type = "simple";
  #     ExecStart = "${pkgs.oguri}/bin/oguri";
  #     RestartSec = 5;
  #     Restart = "always";
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

  systemd.user.services.nextcloud = {
    Unit = {
      Description = "Nextcloud client";
      BindsTo = [ "sway-session.target" ];
      After = [ "sway-session.target" "network.target" ];
      ConditionEnvironment = [ "WAYLAND_DISPLAY" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${pkgs.nextcloud-client}/bin/nextcloud --background";
      ExecReload = "/run/current-system/sw/bin/kill -HUP $MAINPID";
      KillMode = "process";
      RestartSec = 5;
      Restart = "always";
    };
    Install = {
      WantedBy = [ "sway-session.target" ];
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
