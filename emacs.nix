{ pkgs, ... }:
let
  myEmacs = (pkgs.emacs.override {
    withPgtk = true;
  });
in
{
  programs.emacs = {    
    enable = true;
    package = myEmacs;
    extraPackages = epkgs: with epkgs; [
      use-package
    ];
  };

  home.file.".emacs.d/init.el".source = ./dotfiles/init.el;
}
