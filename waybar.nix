{ pkgs, lib, ... }:
{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    systemd.target = "sway-session.target";
    style = ''
* {
    border: none;
    border-radius: 0;
    font-family: "Ubuntu Nerd Font";
    font-size: 13px;
    min-height: 0;
    margin: 1px;
    padding: 0 3px;
}

window#waybar {
    background: transparent;
    color: white;
}

#window {
    font-weight: bold;
    font-family: "Ubuntu";
}
/*
#workspaces {
    padding: 0 5px;
}
*/

#workspaces button {
    padding: 0 5px;
    background: transparent;
    color: white;
    border-top: 2px solid transparent;
}

#workspaces button.focused {
    color: #c9545d;
    border-top: 2px solid #c9545d;
}

#mode {
    background: #64727D;
    border-bottom: 3px solid white;
}

#clock {
    font-weight: bold;
}

#battery {
}

#battery icon {
    color: red;
}

#battery.charging {
}

@keyframes blink {
    to {
        background-color: #ffffff;
        color: black;
    }
}

#battery.warning:not(.charging) {
    color: white;
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
}

#cpu {
}

#memory {
}

#network {
}

#network.disconnected {
    background: #f53c3c;
}

#pulseaudio {
}

#pulseaudio.muted {
}

/* #custom-spotify { */
/*     color: rgb(102, 220, 105); */
/* } */

#tray {
}

    '';
    settings = [{
      height = 30;
      layer = "top";
      position = "top";
      modules-center = [
        # "sway/window"
      ];
      modules-left = [
        "sway/workspaces"
        "sway/language"
        "custom/screen"
        "sway/mode"
        "tray"
      ];
      modules-right = [
        "pulseaudio"
        "network"
        "bluetooth"
        "cpu"
        "memory"
        "disk"
        "backlight"
        "temperature"
        "battery"
        "clock"
        "custom/power"
      ];
      "sway/window" = {
        icon = true;
      };
      backlight = {
		    device = "eDP-1";
		    format = "{percent}% {icon}";
		    format-icons = [ "☼" "☀" ];
	    };
      "custom/power" = {
        format = "";
        on-click = "${pkgs.wlogout}/bin/wlogout -p layer-shell";
      };
      bluetooth = {
        on-click = "${pkgs.blueman}/bin/blueman-manager";
        format = "{status} ";
        tooltip-format = "{controller_alias}\t{controller_address}";
  	    tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{device_enumerate}";
  	    tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
      };
      battery = {
        format = "{capacity}% {icon}";
        format-alt = "{time} {icon}";
        format-charging = "{capacity}% ";
        format-icons = [ "" "" "" "" "" ];
        format-plugged = "{capacity}% ";
        states = {
          critical = 15;
          warning = 30;
        };
      };
      clock = {
        format-alt = "{:%Y-%m-%d}";
        tooltip-format = "{:%Y-%m-%d | %H:%M}";
      };
      cpu = {
        format = "{usage}% ";
        on-click = "${pkgs.alacritty}/bin/alacritty -e ${pkgs.btop}/bin/btop";
        tooltip = true;
      };
      memory = { format = "{}% "; };
      "custom/screen" = {
        format = "⃢";
        on-click = "${pkgs.wdisplays}/bin/wdisplays";
      };
      disk = {
        interval = 30;
        format = "{percentage_free}% 🖴";
        path = "/";
        on-click = "${pkgs.pcmanfm}/bin/pcmanfm /home/kaptch";
      };
      network = {
        interval = 1;
        # format-alt = "{ifname}: {ipaddr}/{cidr}";
        format-disconnected = "Disconnected ⚠";
        format-ethernet = "{ifname}: {ipaddr}/{cidr}   up: {bandwidthUpBits} down: {bandwidthDownBits}";
        format-linked = "{ifname} (No IP) ";
        format-wifi = "{essid} ({signalStrength}%) ";
        on-click = "${pkgs.alacritty}/bin/alacritty -e ${pkgs.networkmanager}/bin/nmtui";
      };
      pulseaudio = {
        format = "{volume}% {icon} {format_source}";
        format-bluetooth = "{volume}% {icon} {format_source}";
        format-bluetooth-muted = " {icon} {format_source}";
        format-icons = {
          car = "";
          default = [ "" "" "" ];
          handsfree = "";
          headphones = "";
          headset = "";
          phone = "";
          portable = "";
        };
        format-muted = " {format_source}";
        format-source = "{volume}% ";
        format-source-muted = "";
        on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
      };
      "sway/mode" = {
        format = " {}";
        max-length = 50;
      };
      temperature = {
        critical-threshold = 80;
        format = "{temperatureC}°C {icon}";
        format-icons = [ "" "" "" ];
      };
      "sway/language" = {
        format = "{short} {variant}";
        on-click = "${pkgs.sway}/bin/swaymsg input type:keyboard xkb_switch_layout next";
      };
    }];
  };
}
