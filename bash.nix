{
  programs.bash = {
    enable = true;
    sessionVariables = { EDITOR = "emacs";
                         COQPATH = "/home/kaptch/.nix-profile/lib/coq/8.14/user-contrib";
                         SSH_AUTH_SOCK = "/run/user/$UID/gnupg/S.gpg-agent.ssh"; };
    initExtra = "test -r /home/kaptch/.opam/opam-init/init.sh && . /home/kaptch/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true";
    # Yubikey support
    shellAliases = {
      ssh = "gpg-connect-agent updatestartuptty /bye > /dev/null; ssh";
      scp = "gpg-connect-agent updatestartuptty /bye > /dev/null; scp";
      mattermost-desktop = "mattermost-desktop -d ~/Mattermost";
    };
  };  
}
