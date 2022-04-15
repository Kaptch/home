{
  programs.git = {
    enable = true;
    userName = "Kaptch";
    userEmail = "kaptch@gmail.com";
    extraConfig = {
      core.editor = "emacs";
      credential.helper = "cache";
    };
  };
}
