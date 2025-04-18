# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{ pkgs, ... }:
{
  nix.settings = {
    # add the hyprland cache so that we dont build hyprland from source
    substituters = [ "https://hyprland.cachix.org" ];
    trusted-public-keys = [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
  };

  # these extra portals allow for things like screen sharing
  xdg = {
    portal = {
      enable = true;
      extraPortals = [
        pkgs.xdg-desktop-portal-wlr
        pkgs.xdg-desktop-portal-gtk
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    hyprland            # the actual package
    tofi                # launcher
    uwsm                # wayland session manager
    hyprland-qtutils    # needed by hyprland
    hyprpolkitagent     # polkit agent
  ];

  # we use uwsm to manage launching hyprland
  # uswm will add hyprland to the login sessions with greetd.
  programs = {
    uwsm.enable = true;
    uwsm.waylandCompositors = {
      hyprland = {
        prettyName = "Hyprland";
        comment = "Hyprland compositor managed by UWSM";
        binPath = "/run/current-system/sw/bin/Hyprland";
      };
    };

    hyprland = {
      withUWSM  = true;
      enable = true;
      xwayland.enable = true;
    };
  };

  # this is mainly for the lock screen
  # lock.png is provided elsewhere
  services.xserver = {
    enable = true;

    desktopManager = {
      xterm.enable = false;
    };


    displayManager = {
      lightdm.background = ./lock.png;
    };
  };

  # this is a lot of env vars.
  # and this requires some cleanup
  # but hyprland moves fast and some of these are probably outdated already
  environment.sessionVariables = {
    XDG_SESSION_TYPE="wayland";
    XDG_CURRENT_DESKTOP="Hyprland";
    XDG_SESSION_DESKTOP="Hyprland";
    NIXOS_OZONE_WL="1";
    GTK_THEME="Nordic";
    XCURSOR_THEME="Nordzy-cursors";
    XCURSOR_SIZE="24";
    HYPRCURSOR_THEME="Nordzy-cursors";
    HYPRCURSOR_SIZE="24";
  };

  # allow hyprlock (lockscreen) to lock user session
  security.pam.services.hyprlock = { };
  security.polkit.enable = true;
  security.pam.services.gdm.enableGnomeKeyring = true;
}
