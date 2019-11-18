{ config, pkgs, lib, ... }:

{
  nixpkgs.config.allowUnfree = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };
  programs.ssh.startAgent = false;

  services.pcscd.enable = true;

  services.udev.packages = [ pkgs.yubikey-personalization ];
  environment.shellInit = ''
    export GPG_TTY="$(tty)"
    gpg-connect-agent /bye
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    # export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
  '';

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # TODO: enable xdg?
  # services.flatpak. enable = true;
  # xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];

  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_11;
  services.postgresql.enableTCPIP = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # TODO: Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  services.xserver.windowManager.i3.enable = true;

  # services.xserver.desktopManager.default = "none";
  # services.xserver.windowMangaer.default = "i3";

  fonts.fonts = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    fantasque-sans-mono
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget vim curl git ag tmux zsh

    neovim # TODO: python support?
    # TODO: npm?
    ranger nodejs python2 python3 python37Packages.virtualenv python37Packages.pip

    go

    rxvt_unicode dmenu polybarFull

    firefox google-chrome
    pass gopass gnupg yubikey-personalization yubikey-personalization-gui
    # TODO: keybase gui https://github.com/danielfullmer/nixos-config/blob/master/profiles/yubikey.nix

    htop
    xclip xsel
    mpv

    zathura

    abook neomutt isync notmuch notmuch-mutt msmtp

    xfce.thunar xfce.xfce4-screenshooter gthumb

    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnome3.zenity
    lxappearance awf
    arandr

    traceroute mtr telnet nmap
    # TODO: bpftrace
  ];

  programs.zsh.enable = true;

  # TODO: figure out how to properly set passwords and then turn off
  # users.mutableUsers = false;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups = [ { name = "darth"; } ];
  users.users.darth = {
    shell = pkgs.zsh;
    isNormalUser = true;
    group = "darth";
    home = "/home/darth";
    createHome = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  users.users.root.initialHashedPassword = "";

  security.sudo.wheelNeedsPassword = false;

  # Taken from https://github.com/danielfullmer/nixos-config/blob/master/profiles/yubikey.nix#L51-L67
  systemd.services.gpg-key-import = {
    description = "Import gpg keys";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "darth";
      Group = "darth";
    };
    script = ''
      ${lib.getBin pkgs.gnupg}/bin/gpg --import ~darth/.dotfiles/keys/yubikey.asc
      ${lib.getBin pkgs.gnupg}/bin/gpg --import-ownertrust << EOF
      47C0838C9E85F2D5E87C7A68DF5E8202BC499DA7:6:
      EOF
    '';
    # TODO: Maybe create a udev rule to run "gpg --card-status" when yubikey plugged in first time
  };
}
