{ config, pkgs, ... }:

{
  # Zsh Shell
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    initExtra = "krabby random -i\n";
    oh-my-zsh = {
      enable = true;
      theme = "afowler";
    };
    shellAliases = {
      d = "doas";
      s = "sudo";

      c = "clear";
      t = "tree";
      vi = "nvim";

      rs-nix = "nix run github:cargo2nix/cargo2nix";
      discordBot = "cd && ./System/Code/wiggler/target/release/wiggler && cd -";
    };
  };

  programs.starship = {
    enable = true;
    package = pkgs.starship;
    enableZshIntegration = true;

    settings = {
      # Inserts a blank line between shell prompts
      add_newline = true;

      # Base Modules
      package = {
        disabled = false;
      };

      cmd_duration = {
        min_time = 500;
        format = "took [$duration](bold yellow)";
      };

      git_branch = {
        symbol = " ";
        style = "bold italic red";
        format = "[git:](bold blue)[\\(](bold blue)[$branch(:$remote_branch)]($style)[\\)](bold blue) ";
      };

      # Programming Modules
      golang = {
        disabled = false;
        symbol = " ";
      };

      haskell = {
        disabled = false;
        symbol = "λ ";
      };
      nim = {
        disabled = false;
        symbol = " "; 
      };

      nix_shell = {
        disabled = false;
        symbol = " ";
      };

      nodejs = {
        disabled = false;
        symbol = " ";
      };

      python = {
        disabled = false;
        symbol = " ";
      };

      rust = {
        disabled = false;
        symbol = " ";
      };
    };
  };
}
