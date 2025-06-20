# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{
  hostname,
  pkgs,
  lib,
  modulesPath,
  user,
  ...
}:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    ../../hardware/hardware.nix
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"       # usb wake up (usb host controller)
    "thunderbolt"    # :/
    "nvme"           # support for the nvme disk in here
    "usb_storage"    # :/
    "sd_mod"         # hard drive controller
  ];

  boot.kernelParams = [ "amd_pstate=active"
                        "acpi.ec_no_wakeup=1"
                        # Force use of the thinkpad_acpi driver for backlight control.
                        # This allows the backlight save/load systemd service to work.
                        "acpi_backlight=native"
                        # Needed for touchpad to work properly (click doesn't register by pushing down the touchpad).
                        "psmouse.synaptics_intertouch=0"
                      ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.useOSProber = true; # detect windows since thats on a partition here
  boot.loader.grub.devices = [ "/dev/nvme0n1" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/0bda9355-76f4-4b55-9012-0a14a73ac6b9";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."luks-f400c0ed-57e0-4b5a-b701-c1a10c19480f".device = "/dev/disk/by-uuid/f400c0ed-57e0-4b5a-b701-c1a10c19480f";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/5A5A-DEFE";
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  # external disk
  fileSystems."/home/${user.username}/external" = {
    device = "/dev/disk/by-uuid/18818348-1ee4-4fa5-9984-e4e01b9fa304";
    fsType = "ext4";
  };

  swapDevices = [];

  hardware.graphics = {
    enable = lib.mkDefault true;
    enable32Bit = lib.mkDefault true;
  };

  hardware.amdgpu.initrd.enable = lib.mkDefault true;
  networking.hostName = hostname;

  # enalbe fingerprinting services
  services.fprintd.enable = true;
}
