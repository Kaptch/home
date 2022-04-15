{ pkgs, ... }:
{
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      use-package
    ];
  };

  home.file.".emacs.d/init.el".source = ./dotfiles/init.el;
}
