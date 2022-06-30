{
  programs.waybar = {
    enable = true;
    systemd.enable = true;
    style = ''
* {
    border: none;
    border-radius: 0;
    font-family: "Ubuntu Nerd Font";
    font-size: 13px;
    min-height: 0;
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

#clock, #battery, #cpu, #memory, #network, #pulseaudio, /* #custom-spotify, */ #tray, #mode {
    padding: 0 3px;
    margin: 0 2px;
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
      modules-center = [ "sway/window" ];
      modules-left = [ "sway/workspaces" "tray" "sway/mode" ];
      modules-right = [
        "wlr/taskbar"        
        "bluetooth"
	      "sway/language"	
        "pulseaudio"
        "network"
        "cpu"
        "memory"
        "temperature"
        "battery"
        "clock"
        "custom/power"
      ];
      "custom/power" = {
        format = "";
        on-click = "wlogout -p layer-shell";
      };
      "wlr/taskbar" = {
	      all-outputs = true;   
  	    format = "{icon}";
  	    icon-size = 14;
  	    icon-theme = "Numix-Circle";
  	    tooltip-format = "{title}";
  	    on-click = "activate";
  	    on-click-middle = "close";
      };
      bluetooth = {
  	    format = "{icon}";
  	    format-alt = "bluetooth: {status}";
  	    format-icons = {
  		    enabled = "";
  		    disabled = "";
  	    };
  	    tooltip-format = "{}";
        on-click = "blueman-manager";
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
        tooltip = false;
      };
      memory = { format = "{}% "; };
      network = {
        interval = 1;
        format-alt = "{ifname}: {ipaddr}/{cidr}";
        format-disconnected = "Disconnected ⚠";
        format-ethernet = "{ifname}: {ipaddr}/{cidr}   up: {bandwidthUpBits} down: {bandwidthDownBits}";
        format-linked = "{ifname} (No IP) ";
        format-wifi = "{essid} ({signalStrength}%) ";
        on-click = "alacritty --hold -e nmtui";
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
        on-click = "pavucontrol";
      };
      "sway/mode" = {
        format = ''<span style="italic">{}</span>'';
      };
      temperature = {
        critical-threshold = 80;
        format = "{temperatureC}°C {icon}";
        format-icons = [ "" "" "" ];
      };
      "sway/language" = {
        format = "{short} {variant}";
        on-click = "swaymsg input type:keyboard xkb_switch_layout next";
      };
    }];
  };
}
