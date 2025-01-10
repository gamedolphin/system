# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{ user, ... } :
{
  imports =
    [
      ./hardware-configuration.nix
      ../../hardware/nvidia.nix
      ../../configuration
    ];

  sops.secrets."ssh/bigbox/private" = { # ssh private key
    owner = "${user.username}";
    mode = "600";
    path = "/home/${user.username}/.ssh/id_ed25519";
  };
  sops.secrets."ssh/bigbox/public" = { # ssh public key
    owner = "${user.username}";
    mode = "644";
    path = "/home/${user.username}/.ssh/id_ed25519.pub";
  };
  sops.secrets."ssh/wavefunk/private" = { # ssh private key
    owner = "${user.username}";
    mode = "600";
    path = "/home/${user.username}/.ssh/wavefunk";
  };
  sops.secrets."ssh/wavefunk/public" = { # ssh public key
    owner = "${user.username}";
    mode = "644";
    path = "/home/${user.username}/.ssh/wavefunk.pub";
  };
  sops.secrets."ssh/wavefunk_dev/private" = { # ssh private key
    owner = "${user.username}";
    mode = "600";
    path = "/home/${user.username}/.ssh/wavefunk_dev";
  };
  sops.secrets."ssh/wavefunk_dev/public" = { # ssh public key
    owner = "${user.username}";
    mode = "644";
    path = "/home/${user.username}/.ssh/wavefunk_dev.pub";
  };
}