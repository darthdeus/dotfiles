{pkgs, ...}: {
  home.username = "darth";
  home.homeDirectory = "/home/darth";

  home.packages = with pkgs; [
    nixpkgs-fmt
    git
    neovim
    zsh
    ranger
    ctags
    htop
    xclip
    xsel
    zathura
    gthumb
    xfce.thunar xfce.xfce4-screenshooter
    curl
    tmux
    mosh
    unzip
    wget
  ];

  home.stateVersion = "23.05";
  programs.home-manager.enable = true;
}
