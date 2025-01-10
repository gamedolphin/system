# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{ pkgs, ...}:
{
  environment.systemPackages = with pkgs; [
    pamixer
  ];
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # pipewire needs realtime scheduling access
  security.rtkit.enable = true;
}