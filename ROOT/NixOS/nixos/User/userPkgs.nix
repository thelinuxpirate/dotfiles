{ config, pkgs, ... }:

{
  services.emacs.defaultEditor = true;
#  services.emacs.package = pkgs.emacsGit;

  users.users.larry.shell = pkgs.zsh;
  
  programs.zsh = {
    ohMyZsh = {
    enable = true;
    plugins = [ "git" "python" "man" ];
    theme = "robbyrussell";
    custom = "/home/larry/.zsh-custom";
    };
    shellAliases = {
      c = "clear";
      t = "tree";
      s = "sudo";
      cD = "cd .. && cd ..";
      gitc = "git clone";
      rebuild = "sudo nixos-rebuild switch";
      tuxfetch = "neofetch --ascii_distro tux";
      neofetch = "neofetch --colors 4 3 5 7 1 2";
      cfetch = "neofetch --jp2a";
      tmacs = "emacs --terminal";
      cmacsT = "emacsclient --create-frame --tty";
      vim = "nvim";
      svim = "sudo nvim";
      snvim = "sudo nvim";
    };
  };
}
