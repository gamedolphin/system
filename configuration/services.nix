# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{
  services = {
    blueman.enable = true;                # bluetooth manager
    fwupd.enable = true;                  # firmware updating service
    fstrim.enable = true;                 # ssd maintenance service
    thermald.enable = true;               # thermal regulation service
    printing.enable = true;               # printing services, cups
    gnome.gnome-keyring.enable = true;    # keyring
    flatpak.enable = true;                # allow installing things from flatpaks

    # printer discovery
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}