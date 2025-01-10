# WARNING : This file was generated by README.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.
{ pkgs, user, ... }:
{
  console.useXkbConfig = true;
  users.users.${user.username}.shell = pkgs.zsh;

  environment.shells = with pkgs; [ zsh ];
  programs.zsh.enable = true;
}
