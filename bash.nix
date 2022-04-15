{
  programs.bash = {
    enable = true;
    sessionVariables = { EDITOR = "emacs"; COQPATH = "/home/kaptch/.nix-profile/lib/coq/8.15/user-contrib"; };
    initExtra = "test -r /home/kaptch/.opam/opam-init/init.sh && . /home/kaptch/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true";
    # Yubikey support
    shellAliases = {
      ssh = "gpg-connect-agent updatestartuptty /bye > /dev/null; ssh";
      scp = "gpg-connect-agent updatestartuptty /bye > /dev/null; scp";
    };
  };  
}
