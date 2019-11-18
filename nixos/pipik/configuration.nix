# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "pipik"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.networks = {
    UPC248346737 = {
      psk = "mamenovybytecek";
    };
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Prague";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget vim curl git ag tmux zsh

    fantasque-sans-mono

    neovim # TODO: python support?
    # TODO: npm?
    ranger nodejs python2 python3

    go

    rxvt_unicode dmenu polybarFull

    firefox google-chrome
    gopass gnupg yubikey-personalization yubikey-personalization-gui
    # TODO: keybase gui https://github.com/danielfullmer/nixos-config/blob/master/profiles/yubikey.nix

    htop
    xclip xsel
    mpv

    zathura

    abook neomutt isync notmuch notmuch-mutt msmtp

    xfce.thunar gthumb

    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    lxappearance awf
    arandr

    # TODO: bpftrace
  ];

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

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  services.xserver.windowManager.i3.enable = true;

  # services.xserver.desktopManager.default = "none";
  # services.xserver.windowMangaer.default = "i3";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups = [ { name = "darth"; } ];
  users.users.darth = {
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


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "20.03"; # Did you read the comment?

}
