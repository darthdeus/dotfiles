{ config, pkgs, ... }:

{
  # TODO: check out https://gist.github.com/mandrean/65108e0898629e20afe1002d8bf4f223

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "darth";
  home.homeDirectory = if pkgs.stdenv.isLinux then "/home/darth" else "/Users/darth";


  home.packages = let 
    noxMaster = pkgs.nox.overrideAttrs (finalAttrs: previousAttrs: {
      patches = [./nox.patch];
    });
  in with pkgs; [
    bc
    bat
    curl
    htop
    gnupg
    pass
    zathura

    nix

    fzf
    fd
    ripgrep
    jq
    tree
    ranger
    xfce.thunar
    rofi

    abook
    dstat
    arandr
    rxvt-unicode
    mpv
    
    neomutt
    lynx
    w3m
    traceroute

    fping
    gnumake

    xclip
    youtube-dl
    feh
    dmenu
    ffmpeg

    noxMaster

    # neovim
    tmux
    # pkgs.nox
    # polybar

    cargo-bloat loc flamegraph wasm-strip simple-http-server
  ];

  targets.genericLinux.enable = pkgs.stdenv.isLinux;

  # programs.mbsync.enable = true;

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;

    initExtra = ''
    bindkey '^ ' autosuggest-accept
    unsetopt beep
    bindkey -e

    autoload -Uz select-word-style
    select-word-style bash

    source ~/.zsh/prompt.zsh

    autoload -U edit-command-line
    zle -N edit-command-line
    bindkey '\C-x\C-e' edit-command-line

    export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git --ignore "*.png" --ignore "*.jpg" --ignore "*.mp3" --ignore "*.import" --ignore "*.wav" --ignore "*.ogg" --ignore "*.aseprite" --ignore "*.ttf" --ignore "*.gif" --ignore "*.TTF" --ignore "*.afdesign" --ignore steam --ignore "*.afphoto" --ignore "*.tres" -l -g ""'
    # export FZF_DEFAULT_COMMAND='
    #   (git ls-tree -r --name-only HEAD ||
    #    find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
    #       sed s/^..//) 2> /dev/null'


    if [ -z "$VIM_VERSION" ]; then
      VIM_VERSION="nvim"
    fi

    export EDITOR="$VIM_VERSION"
    export VISUAL="$VIM_VERSION"

    alias vim="$VIM_VERSION"
    alias vi="vim"

    stty sane

    source ~/.zshrc.dot
    '';

    zplug = {
      enable = true;
      plugins= [
        {
          name = "ytet5uy4/fzf-widgets";
        }
        {
          name = "changyuheng/fz";
        }
        {
          name = "rupa/z";
        }
      ];
    };

    plugins = with pkgs; [

    ];
  };

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
