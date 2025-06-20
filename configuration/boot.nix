# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{ pkgs, ... } :
{
  boot = {
    initrd = {
      verbose = false;     # its a lot of logs. dont need it, unless we do.
      kernelModules = [ ]; # no kernel modules on boot
    };

    extraModulePackages = [ ];                   # no extra packages on boot either
    kernelPackages = pkgs.linuxPackages_latest;  # latest greatest linux kernel
    kernelParams = [ "silent" ];                 # quiet those logs

    consoleLogLevel = 0;                         # quiten more logs
    plymouth.enable = true;                      # graphical boot animation instead

    supportedFilesystems = [ "ntfs" ];           # should see the ntfs (windows)

    loader = {
      systemd-boot.enable = true;                # systemd-boot
      systemd-boot.configurationLimit = 5;
      efi.canTouchEfiVariables = true;           # allow editing efi to edit the boot loader


      timeout = 5;                               # grub timeout to make a selection
    };
  };
}
