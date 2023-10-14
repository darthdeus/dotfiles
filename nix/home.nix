{ pkgs, ... }: {
  home.username = "darth";
  home.homeDirectory = "/home/darth";

  home.packages = with pkgs; [
    nixpkgs-fmt
    git
    ranger
    universal-ctags
    htop
    xclip
    xsel
    zathura
    gthumb
    xfce.thunar
    xfce.xfce4-screenshooter
    curl
    mosh
    unzip
    wget

    delta

    nodejs

    gnumake
    zig
    fzf
    fd
    ripgrep
    jq

    # busybox

    tracy

    clang
    mold
    pkg-config

    nix-index
    # nixos-search
    nixos-option

    rustup
    # rust-analyzer
    # cargo
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  services.sxhkd = {
    enable = true;
    extraConfig = ''
      ${builtins.readFile ~/.dotfiles/sxhkd/sxhkdrc}
    '';
  };

  services.mpd = {
    enable = true;
    musicDirectory = "~/music";
  };

  services.polybar = {
    enable = true;

    script = "~/.dotfiles/polybar/launch.sh";

    package = pkgs.polybar.override {
      i3Support = true;
      pulseSupport = true;
      mpdSupport = true;
      alsaSupport = true;
    };
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    initExtra = ''
      source ~/.dotfiles/zsh/zshrc
    '';
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
      source-file /home/darth/.dotfiles/tmux.conf
    '';
  };

  programs.alacritty.enable = true;
  programs.kitty.enable = true;

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
  };

  home.stateVersion = "23.05";
  programs.home-manager.enable = true;
}
