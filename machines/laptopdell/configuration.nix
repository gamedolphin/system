# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{
  imports =
    [
      ./hardware-configuration.nix
      ../../hardware/nvidia.nix
      ../../configuration
    ];

  environment.sessionVariables = {
    GDK_SCALE = 2;
  };
}
