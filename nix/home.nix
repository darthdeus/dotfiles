{ pkgs, ... }: {
  home.username = "darth";
  home.homeDirectory = "/home/darth";

  home.packages = with pkgs; [
    nixpkgs-fmt
    git
    ranger
    ctags
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

    nodejs

    gnumake
    zig
    fzf
    fd
    ripgrep
    jq

    tracy

    clang
    mold
    pkg-config
    # rustup

    alsaLib.dev
    libxkbcommon
  ];

  # LD_LIBRARY_PATH = with pkgs;
  #   lib.makeLibraryPath [
  #     libxkbcommon # keyboard
  #     wayland
  #     libGL # OpenGL I think
  #     alsaLib # sound
  #   ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

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
