{
  programs.bash = {
    enable = true;
    sessionVariables = { EDITOR = "emacs";
                         COQPATH = "/home/kaptch/.nix-profile/lib/coq/8.16/user-contrib";
                         SSH_AUTH_SOCK = "/run/user/$UID/gnupg/S.gpg-agent.ssh"; };
    # Yubikey support
    shellAliases = {
      ssh = "gpg-connect-agent updatestartuptty /bye > /dev/null; ssh";
      scp = "gpg-connect-agent updatestartuptty /bye > /dev/null; scp";
    };
    bashrcExtra = "gpg-connect-agent updatestartuptty /bye > /dev/null\n";
  };
}
