{ pkgs, lib, config, ... }:
let
  # Initial sway config
  sway-cfg = config.wayland.windowManager.sway.config;
in
{
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = {
      terminal = "alacritty";
      menu = "bemenu-run";
      modifier = "Mod4";
      bars = [];
      startup = [{ command = "mako"; }];
      keybindings =
        let
          screenshot_dir =
            "~/Pictures/Screenshots/$(date +'%Y-%m-%d+%H:%M:%S').png";
        in
          lib.mkOptionDefault {
            "XF86AudioMute" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
            "--locked XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
            "--locked XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +5%";
            "XF86AudioRaiseVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "XF86AudioLowerVolume" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
            "${sway-cfg.modifier}+p" = "exec ${pkgs.wdisplays}/bin/wdisplays";            
            "${sway-cfg.modifier}+l" = "exec ${pkgs.swaylock-fancy}/bin/swaylock-fancy";
	          "${sway-cfg.modifier}+Print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot --notify save screen ${screenshot_dir}";
            "${sway-cfg.modifier}+Shift+Print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot --notify save area ${screenshot_dir}";
            "${sway-cfg.modifier}+Shift+a" = "exec nwggrid";
          };
    };
    # swaybg_command oguri -c ~/.config/oguri/config
    extraConfig = ''
      set $laptop eDP-1
      bindswitch --reload --locked lid:on output $laptop disable
      bindswitch --reload --locked lid:off output $laptop enable
      set $wallpapers_path $HOME/Pictures/wallpapers
      output * bg #000000 solid_color
      output * bg `find $wallpapers_path -type f | shuf -n 1` fill
      default_border none
      # set $opacity 0.9
      # for_window [class=".*"] opacity $opacity
      # for_window [app_id=".*"] opacity $opacity
      for_window [shell=".*"] title_format "%title :: %shell"
      for_window [app_id="firefox"] inhibit_idle fullscreen
      for_window [app_id="Firefox"] inhibit_idle fullscreen
      for_window [app_id="firefox" title="^Picture-in-Picture$"] floating enable, move position 877 450, sticky enable, opacity 1.0
      input * {
        xkb_layout "us,dk,ru"
        xkb_variant "qwerty"
        xkb_options "grp:win_space_toggle"
      }
    '';
  };
}
