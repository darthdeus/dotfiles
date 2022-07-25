{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "darth";
  home.homeDirectory = "/home/darth";

  home.packages = [
    pkgs.htop
    pkgs.fortune
    pkgs.bat
    pkgs.fzf
    pkgs.ripgrep
    pkgs.jq
    pkgs.tree
    pkgs.ranger
    pkgs.nix
    pkgs.gnumake
    pkgs.neovim
  ];

  programs.zsh.enable = true;
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.bat.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # programs.neovim = {
  #   enable = true;
  # };

  # programs.emacs.enable = true;
  # programs.emacs = {
  #   enable = true;
  #   extraPackages = epkgs: [
  #     epkgs.nix-mode
  #     epkgs.magit
  #   ];
  # };

  # programs.git = {
  #   enable = true;
  #   userName = "Jakub Arnold";
  #   userEmail = "darthdeus@gmail.com";
  # };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";
}
